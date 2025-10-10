run_vlap_regressions <- function(
  REG,
  reg_path = "regression",
  table_path = "tables"
) {
  # create output directories if missing
  if (!dir.exists(reg_path)) {
    dir.create(reg_path, recursive = TRUE)
  }

  # create table output directory
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # helper: check ≥10 consecutive years
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
  stations_with_10yrs <- REG |>
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

  # filter REG to qualifying stations
  REG_10yrs <- REG |>
    inner_join(
      stations_with_10yrs |> filter(consecutive_10yrs),
      by = "stationid"
    )

  # loop through stations
  stations <- unique(REG_10yrs$stationid)
  for (st in stations) {
    station_data <- REG_10yrs |> filter(stationid == st)

    # reshape to long
    long_data <- station_data |>
      pivot_longer(
        cols = c(CHL_comp, SPCD_epi, PH_epi, TP_epi, TP_hypo, SECCHI),
        names_to = "parameter",
        values_to = "value"
      )

    # regression per parameter
    reg_summary <- long_data |>
      group_by(parameter) |>
      summarise(
        non_na_n = sum(!is.na(value)),
        model = list(
          if (sum(!is.na(value)) >= 10) {
            lm(value ~ Year, data = cur_data())
          } else {
            NULL
          }
        ),
        .groups = "drop"
      ) |>
      mutate(
        results = map(
          model,
          ~ if (!is.null(.x)) {
            broom::tidy(.x)
          } else {
            tibble(
              term = "Year",
              estimate = NA,
              std.error = NA,
              statistic = NA,
              p.value = NA
            )
          }
        )
      ) |>
      unnest(results) |>
      filter(term == "Year") |>
      select(parameter, estimate, std.error, statistic, p.value, non_na_n) |>
      mutate(
        significant = !is.na(p.value) & p.value < 0.05,
        slope_dir = case_when(
          !significant ~ NA_character_, # only assign direction if significant
          estimate > 0 ~ "increasing",
          estimate < 0 ~ "decreasing",
          TRUE ~ "none"
        ),
        trend = case_when(
          !significant ~ "Stable", # changed from "not significant"
          parameter %in%
            c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "increasing" ~
            "Worsening",
          parameter %in%
            c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "decreasing" ~
            "Improving",
          parameter %in% c("PH_epi", "SECCHI") & slope_dir == "increasing" ~
            "Improving",
          parameter %in% c("PH_epi", "SECCHI") & slope_dir == "decreasing" ~
            "Worsening",
          TRUE ~ "insufficient data"
        )
      )

    # calculate mean per parameter for % change
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

    # add percent change per year
    reg_summary <- reg_summary |>
      left_join(mean_vals, by = "parameter") |>
      mutate(percent_change = (estimate / mean_value) * 100)

    # save one CSV per station
    write_csv(
      reg_summary,
      file.path(reg_path, paste0("Regression_", st, ".csv"))
    )

    display_table <- reg_summary |>
      mutate(
        Parameter = recode(
          parameter,
          "SPCD_epi" = "Conductivity",
          "CHL_comp" = "Chlorophyll-a",
          "PH_epi" = "pH (epilimnion)",
          "SECCHI" = "Transparency",
          "TP_hypo" = "Phosphorus (hypolimnion)",
          "TP_epi" = "Phosphorus (epilimnion)"
        )
      ) |>
      select(Parameter, Trend = trend)

    # save formatted summary
    write_csv(
      display_table,
      file.path(table_path, paste0("TrendSummary_", st, ".csv"))
    )
  }
}
