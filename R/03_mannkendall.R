run_vlap_mannkendall <- function(
  data_wide,
  mk_path = "mannkendall",
  table_path = "tables"
) {
  # create output directories if they don't exist
  if (!dir.exists(mk_path)) {
    dir.create(mk_path, recursive = TRUE)
  }
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # get list of stations and parameters
  stations <- sort(unique(data_wide$STATIONID))
  parameters <- c(
    "SPCD_epi",
    "CHL_comp",
    "PH_epi",
    "SECCHI",
    "TP_hypo",
    "TP_epi"
  )

  results_list <- list()

  # loop through each station
  for (st in stations) {
    station_data <- data_wide |> filter(STATIONID == st)
    yrs <- sort(unique(station_data$year))

    # check for minimum consecutive years (10)
    consec_lengths <- rle(diff(yrs) == 1)
    max_consec <- if (length(consec_lengths$lengths) > 0) {
      max(consec_lengths$lengths[consec_lengths$values == TRUE]) + 1
    } else {
      1
    }

    if (max_consec >= 10) {
      for (param in parameters) {
        param_data <- station_data |>
          select(STARTDATE, all_of(param)) |>
          arrange(STARTDATE) |>
          mutate(temp = as.numeric(.data[[param]])) |>
          drop_na(temp)

        # run MK test if at least 5 data points
        if (nrow(param_data) >= 5) {
          mk <- mk.test(param_data$temp)
          sen <- sens.slope(param_data$temp)

          # classify trend
          significant <- !is.na(mk$p.value) & mk$p.value < 0.05
          trend_cat <- case_when(
            !significant ~ "Stable",
            param %in%
              c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
              mk$estimates[["tau"]] > 0 ~ "Worsening",
            param %in%
              c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
              mk$estimates[["tau"]] < 0 ~ "Improving",
            param %in%
              c("PH_epi", "SECCHI") &
              mk$estimates[["tau"]] > 0 ~ "Improving",
            param %in%
              c("PH_epi", "SECCHI") &
              mk$estimates[["tau"]] < 0 ~ "Worsening",
            TRUE ~ "Stable"
          )

          results_list[[length(results_list) + 1]] <- tibble(
            STATIONID = st,
            parameter = param,
            n = nrow(param_data),
            tau = mk$estimates[["tau"]],
            mk_p = mk$p.value,
            sen_slope = sen$estimates[["Sen's slope"]],
            sen_ci_lower = sen$conf.int[1],
            sen_ci_upper = sen$conf.int[2],
            trend = trend_cat
          )
        }
      }
    } else {
      message(st, " skipped: less than 10 consecutive years")
    }
  }

  # combine results
  mk_summary <- bind_rows(results_list)

  # write one full MK csv per station
  mk_summary |>
    group_by(STATIONID) |>
    group_split() |>
    walk(function(df) {
      st <- unique(df$STATIONID)
      write_csv(df, file.path(mk_path, paste0("MannKendall_", st, ".csv")))
    })

  # write one display table per station
  mk_summary |>
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
    select(STATIONID, PARAMETER, TREND = trend) |>
    group_by(STATIONID) |>
    group_split() |>
    walk(function(df) {
      st <- unique(df$STATIONID)
      write_csv(
        df |> select(PARAMETER, TREND),
        file.path(table_path, paste0("MK_TrendSummary_", st, ".csv"))
      )
    })

  invisible(mk_summary)
}
