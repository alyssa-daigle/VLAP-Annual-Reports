run_vlap_mannkendall <- function(
  REG,
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

  # check ≥10 consecutive years
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

    # Mann-Kendall per parameter
    mk_summary <- long_data |>
      group_by(parameter) |>
      summarise(
        non_na_n = sum(!is.na(value)),
        MK = list(
          if (sum(!is.na(value)) >= 10) {
            Kendall::MannKendall(value)
          } else {
            NULL
          }
        ),
        SEN = list(
          if (sum(!is.na(value)) >= 10) {
            trend::sens.slope(Year, value)
          } else {
            NULL
          }
        ),
        .groups = "drop"
      ) |>
      mutate(
        tau = map_dbl(MK, ~ if (!is.null(.x)) .x$tau else NA_real_),
        p.value = map_dbl(MK, ~ if (!is.null(.x)) .x$sl else NA_real_),
        slope = map_dbl(SEN, ~ if (!is.null(.x)) .x$estimates else NA_real_)
      ) |>
      mutate(
        significant = !is.na(p.value) & p.value < 0.05,
        slope_dir = case_when(
          !significant ~ NA_character_,
          slope > 0 ~ "increasing",
          slope < 0 ~ "decreasing",
          TRUE ~ "none"
        ),
        trend = case_when(
          !significant ~ "Stable",
          parameter %in%
            c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "increasing" ~ "Worsening",
          parameter %in%
            c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "decreasing" ~ "Improving",
          parameter %in% c("PH_epi", "SECCHI") & slope_dir == "increasing" ~
            "Improving",
          parameter %in% c("PH_epi", "SECCHI") & slope_dir == "decreasing" ~
            "Worsening",
          TRUE ~ "insufficient data"
        )
      )

    # calculate mean per parameter for % change (slope/mean)
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

    # create summary table
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
