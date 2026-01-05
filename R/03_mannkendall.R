run_vlap_mannkendall <- function(
  REG_MK,
  mk_path = "mannkendall",
  table_path = "tables_mk"
) {
  # create output directories if missing
  if (!dir.exists(mk_path)) {
    dir.create(mk_path, recursive = TRUE)
  }
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # check for stations with ≥10 consecutive years
  has_10_consecutive_years <- function(years) {
    years <- sort(unique(years))
    if (length(years) < 10) {
      return(FALSE)
    }
    diffs <- c(0, diff(years))
    runs <- rle(diffs == 1)
    max_run <- max(runs$lengths[runs$values], 0)
    return(max_run + 1 >= 10)
  }

  # stations with ≥10 consecutive years
  stations_with_10yrs <- REG_MK |>
    group_by(stationid) |>
    summarise(
      consecutive_10yrs = has_10_consecutive_years(Year),
      .groups = "drop"
    )

  # save stations without 10 yrs
  stations_needing_note <- stations_with_10yrs |> filter(!consecutive_10yrs)
  write_csv(
    stations_needing_note,
    file.path(table_path, "Stations_less_than_10yrs.csv")
  )

  # filter REG_MK to qualifying stations
  REG_MK_10yrs <- REG_MK |>
    inner_join(
      stations_with_10yrs |> filter(consecutive_10yrs),
      by = "stationid"
    )

  stations <- sort(unique(REG_MK_10yrs$stationid))

  # loop through all stations
  for (st in stations) {
    station_data <- REG_MK_10yrs |> filter(stationid == st)

    # reshape to long format for parameter-wise analysis
    long_data <- station_data |>
      pivot_longer(
        cols = c(CHL_comp, SPCD_epi, PH_epi, TP_epi, TP_hypo, SECCHI),
        names_to = "parameter",
        values_to = "value"
      )

    # compute MK and Sen's slope per parameter
    mk_summary <- long_data |>
      group_by(parameter) |>
      summarise(
        non_na_n = sum(!is.na(value)),

        # Mann-Kendall test (Kendall::MannKendall) is used here to:
        # 1) Calculate 'tau', the rank correlation coefficient, indicating direction/strength of monotonic trend
        # 2) Calculate 'sl' (p-value) to test whether the observed trend is statistically significant
        tau = if (non_na_n >= 10) {
          Kendall::MannKendall(value[!is.na(value)])$tau
        } else {
          NA_real_
        },
        p.value = if (non_na_n >= 10) {
          Kendall::MannKendall(value[!is.na(value)])$sl
        } else {
          NA_real_
        },

        # Sen's slope (trend::sens.slope) estimates the magnitude of change per year
        # This gives the rate at which the parameter increases or decreases over time
        slope = if (non_na_n >= 10) {
          trend::sens.slope(value[!is.na(value)], Year[!is.na(value)])$estimates
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(
        # classify significance based on MK p-value
        significant = !is.na(p.value) & p.value < 0.05,

        # determine slope direction if significant
        slope_dir = case_when(
          slope > 0 ~ "increasing",
          slope < 0 ~ "decreasing",
          TRUE ~ NA_character_
        ),

        # map slope direction into context-specific trend labels
        trend = case_when(
          # significant trends
          significant &
            parameter %in% c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "increasing" ~ "Worsening (significant)",
          significant &
            parameter %in% c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "decreasing" ~ "Improving (significant)",
          significant &
            parameter %in% c("PH_epi", "SECCHI") &
            slope_dir == "increasing" ~ "Improving (significant)",
          significant &
            parameter %in% c("PH_epi", "SECCHI") &
            slope_dir == "decreasing" ~ "Worsening (significant)",

          # weak trends (non-significant)
          !significant &
            parameter %in% c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "increasing" ~ "Worsening (weak)",
          !significant &
            parameter %in% c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "decreasing" ~ "Improving (weak)",
          !significant &
            parameter %in% c("PH_epi", "SECCHI") &
            slope_dir == "increasing" ~ "Improving (weak)",
          !significant &
            parameter %in% c("PH_epi", "SECCHI") &
            slope_dir == "decreasing" ~ "Worsening (weak)",

          # no slope or NA slope
          TRUE ~ "Stable"
        )
      )

    # calculate mean per parameter for percent change calculation
    mean_vals <- station_data |>
      summarise(across(
        c(CHL_comp, SPCD_epi, PH_epi, TP_epi, TP_hypo, SECCHI),
        mean,
        na.rm = TRUE
      )) |>
      pivot_longer(
        everything(),
        names_to = "parameter",
        values_to = "mean_value"
      )

    mk_summary <- mk_summary |>
      left_join(mean_vals, by = "parameter") |>
      mutate(percent_change = (slope / mean_value) * 100)

    # save CSV
    write_csv(
      mk_summary,
      file.path(mk_path, paste0("MannKendall_", st, ".csv"))
    )

    # create simplified trend table for reporting
    display_table <- mk_summary |>
      mutate(
        PARAMETER = recode(
          parameter,
          "SPCD_epi" = "Conductivity",
          "CHL_comp" = "Chlorophyll-a",
          "PH_epi" = "pH (epilimnion)",
          "SECCHI" = "Transparency",
          "TP_hypo" = "Phosphorus (hypolimnion)",
          "TP_epi" = "Phosphorus (epilimnion)"
        )
      ) |>
      select(PARAMETER, TREND = trend)

    write_csv(
      display_table,
      file.path(table_path, paste0("MK_TrendSummary_", st, ".csv"))
    )
  }
}
