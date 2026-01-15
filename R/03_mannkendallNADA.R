run_vlap_mannkendallNADA2 <- function(
  REG_NADA,
  mk_path = "mannkendall",
  table_path = "tables_mk"
) {
  # ---- Create output directories if they do not exist ----
  if (!dir.exists(mk_path)) {
    dir.create(mk_path, recursive = TRUE)
  }
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # ------------------------------------------------------------------
  # Helper: Determine if a station has ≥10 consecutive years
  # This is ONLY for eligibility screening.
  # MK slope/significance will later use all available years for qualifying stations.
  # ------------------------------------------------------------------
  has_10_consecutive_years <- function(years) {
    years <- sort(unique(years))
    if (length(years) < 10) {
      return(FALSE)
    }

    # Calculate differences between consecutive years
    diffs <- c(0, diff(years))
    runs <- rle(diffs == 1)
    max_run <- max(runs$lengths[runs$values], 0)

    # If any run of consecutive years >= 10, station is eligible
    max_run + 1 >= 10
  }

  # ------------------------------------------------------------------
  # Identify stations that meet the ≥10 consecutive year requirement
  # ------------------------------------------------------------------
  stations_with_10yrs <- REG_NADA |>
    group_by(stationid) |>
    summarise(
      consecutive_10yrs = has_10_consecutive_years(Year),
      .groups = "drop"
    )

  # Save a list of stations that fail eligibility for reference
  write_csv(
    stations_with_10yrs |> filter(!consecutive_10yrs),
    file.path(table_path, "Stations_less_than_10yrs.csv")
  )

  # Keep ALL data for eligible stations, including gaps
  # Important: We do NOT filter to only the 10-year run; gaps are preserved to utilize all available data
  REG_NADA_eligible <- REG_NADA |>
    semi_join(
      stations_with_10yrs |> filter(consecutive_10yrs),
      by = "stationid"
    )

  stations <- sort(unique(REG_NADA_eligible$stationid))

  # ------------------------------------------------------------------
  # Loop through each eligible station
  # ------------------------------------------------------------------
  for (st in stations) {
    # show station being processed
    message("Processing station: ", st)

    station_data <- REG_NADA_eligible |> filter(stationid == st)

    # Pivot to long format to analyze each parameter separately
    long_data <- station_data |>
      pivot_longer(
        cols = c(CHL_comp, SPCD_epi, PH_epi, TP_epi, TP_hypo, SECCHI),
        names_to = "parameter",
        values_to = "value"
      )

    # ---- Create ycen column for NADA2 ----
    # TP parameters: use actual censoring information (ND = TRUE)
    # All other parameters: ycen = FALSE (uncensored)
    long_data <- long_data |>
      mutate(
        ycen = case_when(
          parameter == "TP_epi" ~ TP_cens_epi,
          parameter == "TP_hypo" ~ TP_cens_hypo,
          TRUE ~ FALSE
        )
      )

    # ------------------------------------------------------------------
    # Run Mann-Kendall + Sen's slope using NADA2 for all parameters
    # ------------------------------------------------------------------
    mk_summary <- long_data |>
      group_split(parameter) |>
      map_dfr(function(df) {
        param <- df$parameter[1]

        # Remove rows with missing values or missing censoring info
        df <- df |> filter(!is.na(value), !is.na(ycen))
        n_obs <- nrow(df)

        # Guard against insufficient data (minimum 10 observations required)
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

        # ---- Run NADA2 cenken ----
        # Works for both censored (TP) and uncensored (all other parameters)
        # Slopes are calculated per calendar year
        # Gaps in years are preserved
        df$ycen <- as.logical(df$ycen)

        mk <- NADA2::cenken(
          y = df$value,
          ycen = df$ycen,
          x = df$Year
        )

        # ---- Trend classification ----
        # Significant if p < 0.05
        # Tau direction determines Improving/Worsening based on parameter type
        significant <- !is.na(mk$p) & mk$p < 0.05
        trend_cat <- case_when(
          !significant ~ "Stable",
          param %in%
            c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            mk$tau > 0 ~ "Worsening",
          param %in%
            c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            mk$tau < 0 ~ "Improving",
          param %in% c("PH_epi", "SECCHI") & mk$tau > 0 ~ "Improving",
          param %in% c("PH_epi", "SECCHI") & mk$tau < 0 ~ "Worsening",
          TRUE ~ "Stable"
        )

        tibble(
          parameter = param,
          non_na_n = n_obs,
          tau = mk$tau,
          p.value = mk$p,
          slope = mk$slope, # slope per calendar year
          trend = trend_cat
        )
      })

    # ---- Add mean values for percent change calculation ----
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

    # ---- Write full MK summary CSV ----
    write_csv(
      mk_summary,
      file.path(mk_path, paste0("NADA2MannKendall_", st, ".csv"))
    )

    # ---- Write simplified trend summary for reporting ----
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
      file.path(table_path, paste0("NADA2MK_TrendSummary_", st, ".csv"))
    )
  }

  message("Mann-Kendall analysis complete for eligible stations.")
}
