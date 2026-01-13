run_vlap_mannkendallNADA2 <- function(
  REG_NADA,
  mk_path = "mannkendall",
  table_path = "tables_mk"
) {
  if (!dir.exists(mk_path)) {
    dir.create(mk_path, recursive = TRUE)
  }
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # ---- Helper: check for 10 consecutive years ----
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

  # ---- Identify eligible stations ----
  stations_with_10yrs <- REG_NADA |>
    group_by(stationid) |>
    summarise(
      consecutive_10yrs = has_10_consecutive_years(Year),
      .groups = "drop"
    )

  write_csv(
    stations_with_10yrs |> filter(!consecutive_10yrs),
    file.path(table_path, "Stations_less_than_10yrs.csv")
  )

  eligible_stations <- stations_with_10yrs |>
    filter(consecutive_10yrs) |>
    pull(stationid)

  # ---- Loop through stations ----
  for (st in eligible_stations) {
    message("Processing station: ", st)

    station_data <- REG_NADA |> filter(stationid == st)

    # ---- Pivot longer for parameter-wise MK ----
    long_data <- station_data |>
      pivot_longer(
        cols = c(CHL_comp, SPCD_epi, PH_epi, TP_epi, TP_hypo, SECCHI),
        names_to = "parameter",
        values_to = "value"
      )

    mk_summary <- map_dfr(unique(long_data$parameter), function(param) {
      df <- long_data |> filter(parameter == param) |> select(Year, value)

      # ---- Remove NAs ----
      df <- df |> filter(!is.na(value))

      n_obs <- nrow(df)
      if (n_obs < 10) {
        return(tibble(
          parameter = param,
          non_na_n = n_obs,
          tau = NA_real_,
          p.value = NA_real_,
          slope = NA_real_,
          trend = "insufficient data"
        ))
      }

      df <- df |> arrange(Year)
      df$Year <- as.numeric(df$Year)

      # ---- Run MK ----
      mk <- Kendall::MannKendall(df$value)
      tau <- mk$tau
      pval <- mk$sl
      slope <- trend::sens.slope(df$value, df$Year)$estimates

      # ---- Trend category ----
      significant <- !is.na(pval) & pval < 0.05
      trend_cat <- case_when(
        !significant ~ "Stable",
        param %in%
          c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
          tau > 0 ~ "Worsening",
        param %in%
          c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
          tau < 0 ~ "Improving",
        param %in% c("PH_epi", "SECCHI") & tau > 0 ~ "Improving",
        param %in% c("PH_epi", "SECCHI") & tau < 0 ~ "Worsening",
        TRUE ~ "Stable"
      )

      tibble(
        parameter = param,
        non_na_n = n_obs,
        tau = tau,
        p.value = pval,
        slope = slope,
        trend = trend_cat
      )
    })

    # ---- Add mean values and percent change ----
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

    # ---- Write CSVs ----
    write_csv(
      mk_summary,
      file.path(mk_path, paste0("MannKendall_", st, ".csv"))
    )

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

  message("Mann-Kendall analysis complete for eligible stations.")
}
