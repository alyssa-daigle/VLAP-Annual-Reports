run_vlap_mannkendall <- function(
  data_year_median,
  mk_path = "mannkendall",
  table_path = "tables"
) {
  if (!dir.exists(mk_path)) {
    dir.create(mk_path, recursive = TRUE)
  }
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  stations <- sort(unique(data_year_median$STATIONID))

  keep_params <- c(
    "SPCD_epi",
    "CHL_comp",
    "PH_epi",
    "SECCHI_NVS",
    "TP_hypo",
    "TP_epi"
  )

  data_year_median <- data_year_median |>
    select(STATIONID, year, all_of(keep_params))

  parameters <- setdiff(
    names(data_year_median),
    c("STATIONID", "year")
  )

  results_list <- list()

  for (st in stations) {
    station_data <- data_year_median |> filter(STATIONID == st)

    for (param in parameters) {
      # Get all years present for this station, regardless of NA values
      yrs <- sort(unique(station_data$year))

      if (length(yrs) < 10) {
        message(st, " ", param, " skipped: <10 total years")
        next
      }

      # consecutive-year check (ignore NA in param values)
      consec <- rle(diff(yrs) == 1)
      max_consec <- if (any(consec$values)) {
        max(consec$lengths[consec$values]) + 1
      } else {
        1
      }

      if (max_consec < 10) {
        message(
          st,
          " ",
          param,
          " skipped: longest run = ",
          max_consec,
          " years"
        )
        next
      }

      # Pass **all rows** (with NAs) to the MK test, drop NA inside the test
      param_data <- station_data |> select(year, all_of(param)) |> arrange(year)
      temp <- param_data[[param]]
      temp <- temp[!is.na(temp)] # drop NA only for the MK test

      if (length(temp) < 5) {
        # MK needs at least 5 points
        message(st, " ", param, " skipped: <5 non-NA values")
        next
      }

      mk <- mk.test(temp)
      sen <- sens.slope(temp)

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
          c("PH_epi", "SECCHI_NVS") &
          mk$estimates[["tau"]] > 0 ~ "Improving",
        param %in%
          c("PH_epi", "SECCHI_NVS") &
          mk$estimates[["tau"]] < 0 ~ "Worsening",
        TRUE ~ "Stable"
      )

      results_list[[length(results_list) + 1]] <- tibble(
        STATIONID = st,
        parameter = param,
        n = length(temp),
        tau = mk$estimates[["tau"]],
        mk_p = mk$p.value,
        sen_slope = sen$estimates[["Sen's slope"]],
        sen_ci_lower = sen$conf.int[1],
        sen_ci_upper = sen$conf.int[2],
        trend = trend_cat
      )
    }
  }

  mk_summary <- bind_rows(results_list)

  mk_summary |>
    group_by(STATIONID) |>
    group_split() |>
    walk(function(df) {
      st <- unique(df$STATIONID)
      write_csv(df, file.path(mk_path, paste0("MannKendall_", st, ".csv")))
    })

  mk_summary |>
    mutate(
      PARAMETER = recode(
        parameter,
        "SPCD_epi" = "Conductivity",
        "CHL_comp" = "Chlorophyll-a",
        "PH_epi" = "pH (epilimnion)",
        "SECCHI_NVS" = "Transparency",
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
