library(dplyr)
library(extrafont)
loadfonts(device = "win") # Ensure Calibri is available
par(family = "Calibri") # Base R font

# ---- Data cleaning: filter Snake River ----
snakeriver_data <- data_plot |>
  filter(startsWith(STATIONID, "SNANWH")) |>
  select(where(~ !all(is.na(.))))

snake_avg <- snakeriver_data |>
  group_by(year) |>
  summarise(
    cond = mean(SPCD_trib, na.rm = TRUE),
    chloride = mean(chloride_trib, na.rm = TRUE),
    tp = mean(TP_trib, na.rm = TRUE),
    turb = mean(turb_trib, na.rm = TRUE)
  ) |>
  arrange(year)


### CURRENT YEAR AVERAGES FOR SNAKE RIVER
# ---- Output path ----
TABLE_PATH <- Sys.getenv("TABLE_PATH")

if (!dir.exists(TABLE_PATH)) {
  dir.create(TABLE_PATH, recursive = TRUE)
}

# ---- Filter to 2025 and target stations ----
snake_2025 <- snakeriver_data |>
  filter(
    year == 2025,
    grepl("^SNANWH", STATIONID)
  )

# ---- Calculate per-station averages ----
snake_station_avg <- snake_2025 |>
  group_by(STATIONID) |>
  summarise(
    `Conductivity (μS/cm)` = mean(SPCD_trib, na.rm = TRUE),
    `Chloride (mg/L)` = mean(chloride_trib, na.rm = TRUE),
    `Total Phosphorus (μg/L)` = mean(TP_trib, na.rm = TRUE),
    `Turbidity (NTU)` = mean(turb_trib, na.rm = TRUE),
    pH = mean(PH_trib, na.rm = TRUE),
    n_samples = sum(
      !is.na(SPCD_trib) |
        !is.na(chloride_trib) |
        !is.na(TP_trib) |
        !is.na(turb_trib) |
        !is.na(PH_trib)
    ),
    .groups = "drop"
  ) |>
  arrange(STATIONID)

# ---- Round numeric values ----
snake_station_avg <- snake_station_avg |>
  mutate(across(where(is.numeric), ~ round(., 2)))

# ---- Save table ----
write_csv(
  snake_station_avg,
  file.path(TABLE_PATH, "CYA_SNAKE_RIVER_NEW_HAMPTON.csv"),
  na = "-"
)


### MANN KENDALL FOR SNAKE RIVER
snake_medians <- snakeriver_data |>
  group_by(year) |>
  summarise(
    cond = median(SPCD_trib, na.rm = TRUE),
    chloride = median(chloride_trib, na.rm = TRUE),
    tp = median(TP_trib, na.rm = TRUE),
    turb = median(turb_trib, na.rm = TRUE),
    ph = median(PH_trib, na.rm = TRUE)
  ) |>
  arrange(year)

# ---- Output path from environment ----
TABLE_PATH <- Sys.getenv("TABLE_PATH")
MK_PATH <- Sys.getenv("MK_PATH")

if (!dir.exists(TABLE_PATH)) {
  dir.create(TABLE_PATH, recursive = TRUE)
}

# ---- Parameters to analyze ----
params <- c("cond", "chloride", "tp", "turb", "ph")

# ---- Run Mann-Kendall ----
results_list <- list()

for (param in params) {
  df <- snake_medians |>
    select(year, all_of(param)) |>
    arrange(year)

  temp <- na.omit(df[[param]])

  # ---- Data sufficiency checks ----
  if (length(temp) < 5) {
    message(param, " skipped: <5 values")
    next
  }

  yrs <- df$year[!is.na(df[[param]])]

  if (length(yrs) < 10) {
    message(param, " skipped: <10 years")
    next
  }

  consec <- rle(diff(yrs) == 1)
  max_consec <- if (any(consec$values)) {
    max(consec$lengths[consec$values]) + 1
  } else {
    1
  }

  if (max_consec < 10) {
    message(param, " skipped: insufficient consecutive years")
    next
  }

  # ---- MK + Sen ----
  mk <- mk.test(temp)
  sen <- sens.slope(temp)

  tau <- mk$estimates[["tau"]]
  pval <- mk$p.value

  significant <- !is.na(pval) & pval < 0.05
  marginal <- !is.na(pval) & pval >= 0.05 & pval < 0.1

  # ---- Trend classification (tailored to your params) ----
  trend_cat <- case_when(
    !significant & !marginal ~ "Stable",

    # Nutrient / impairment indicators (higher = worse)
    param %in%
      c("tp", "turb", "cond", "chloride") &
      marginal &
      tau > 0 ~ "Slightly Worsening",
    param %in%
      c("tp", "turb", "cond", "chloride") &
      marginal &
      tau < 0 ~ "Slightly Improving",
    param %in%
      c("tp", "turb", "cond", "chloride") &
      significant &
      tau > 0 ~ "Worsening",
    param %in%
      c("tp", "turb", "cond", "chloride") &
      significant &
      tau < 0 ~ "Improving",

    # pH (higher ~ improving, generally)
    param == "ph" & marginal & tau > 0 ~ "Slightly Improving",
    param == "ph" & marginal & tau < 0 ~ "Slightly Worsening",
    param == "ph" & significant & tau > 0 ~ "Improving",
    param == "ph" & significant & tau < 0 ~ "Worsening",

    TRUE ~ "Stable"
  )

  results_list[[param]] <- tibble(
    Parameter = param,
    n = length(temp),
    tau = tau,
    mk_p = pval,
    sen_slope = sen$estimates[["Sen's slope"]],
    sen_ci_lower = sen$conf.int[1],
    sen_ci_upper = sen$conf.int[2],
    Trend = trend_cat
  )
}

# ---- Combine results ----
mk_summary <- bind_rows(results_list)

# ---- Clean parameter names for table ----
mk_summary <- mk_summary |>
  mutate(
    Parameter = recode(
      Parameter,
      cond = "Conductivity",
      chloride = "Chloride",
      tp = "Total Phosphorus",
      turb = "Turbidity",
      ph = "pH"
    )
  )

# ---- Save table ----
write_csv(
  mk_summary |> select(Parameter, Trend),
  file.path(TABLE_PATH, "MK_TrendSummary_SnakeRiver.csv")
)

# optional: save full stats table too
write_csv(
  mk_summary,
  file.path(MK_PATH, "MK_FullResults_SnakeRiver.csv")
)
