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

  has_10_consecutive_years <- function(years) {
    years <- sort(unique(years))
    if (length(years) < 10) {
      return(FALSE)
    }

    diffs <- c(0, diff(years))
    runs <- rle(diffs == 1)
    max_run <- max(runs$lengths[runs$values], 0)

    max_run + 1 >= 10
  }

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

  REG_NADA_10yrs <- REG_NADA |>
    inner_join(
      stations_with_10yrs |> filter(consecutive_10yrs),
      by = "stationid"
    )

  stations <- unique(REG_NADA_10yrs$stationid)

  for (st in stations) {
    station_data <- REG_NADA_10yrs |> filter(stationid == st)

    long_data <- station_data |>
      pivot_longer(
        cols = c(CHL_comp, SPCD_epi, PH_epi, TP_epi, TP_hypo, SECCHI),
        names_to = "parameter",
        values_to = "value"
      )

    mk_summary <- long_data |>
      group_split(parameter) |>
      map_dfr(function(df) {
        param <- df$parameter[1]

        if (param %in% c("TP_epi", "TP_hypo")) {
          cen_col <- if (param == "TP_epi") "TP_cens_epi" else "TP_cens_hypo"
          df <- df |> filter(!is.na(value) & !is.na(.data[[cen_col]]))
        } else {
          df <- df |> filter(!is.na(value))
        }

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

        # ---- run MK ----
        if (param %in% c("TP_epi", "TP_hypo")) {
          cen_col <- if (param == "TP_epi") "TP_cens_epi" else "TP_cens_hypo"
          df[[cen_col]] <- as.logical(df[[cen_col]])

          mk <- NADA2::cenken(
            y = df$value,
            ycen = df[[cen_col]],
            x = df$Year
          )

          tau <- mk$tau
          slope <- mk$slope
          pval <- mk$p
        } else {
          mk <- Kendall::MannKendall(df$value)

          tau <- mk$tau
          pval <- mk$sl
          slope <- trend::sens.slope(df$value, df$Year)$estimates
        }

        # ---- significance gate ----
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
}
