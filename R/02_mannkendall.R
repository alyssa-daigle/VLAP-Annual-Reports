run_vlap_mannkendall <- function(
  data_year_median,
  mk_path = "mannkendall",
  table_path = "tables"
) {
  # --- Create directories if needed ---
  if (!dir.exists(mk_path)) {
    dir.create(mk_path, recursive = TRUE)
  }
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # --- Define SUNSUN station groups ---
  sunsun_nearshore_stations <- c(
    "SUNSUN010",
    "SUNSUN020",
    "SUNSUN030",
    "SUNSUN040",
    "SUNSUN050",
    "SUNSUN060",
    "SUNSUN070",
    "SUNSUN080",
    "SUNSUN090",
    "SUNSUN1001",
    "SUNSUN110"
  )

  sunsun_deep_stations <- c(
    "SUNSUN1D",
    "SUNSUN220D",
    "SUNSUN2D",
    "SUNSUN3D"
  )

  sunsun_trib_stations <- c(
    "SUNSUN575",
    "SUNSUN610",
    "SUNSUN6401"
  )

  sunsun_all <- c(
    sunsun_nearshore_stations,
    sunsun_deep_stations,
    sunsun_trib_stations
  )

  # --- VLAP parameters to keep ---
  keep_params <- c(
    "SPCD_epi",
    "CHL_comp",
    "PH_epi",
    "SECCHI",
    "SECCHI_NVS",
    "TP_epi",
    "TP_hypo"
  )

  # Filter columns
  data_year_median <- data_year_median |>
    select(STATIONID, STATNAM, year, all_of(keep_params))

  # --- Initialize results list ---
  results_list <- list()
  stations <- sort(unique(data_year_median$STATIONID))

  for (st in stations) {
    station_data <- data_year_median |> filter(STATIONID == st)

    # Skip stations that are not SUNSUN or DEEP
    if (
      !(st %in%
        sunsun_all ||
        any(grepl("DEEP", station_data$STATNAM, ignore.case = TRUE)))
    ) {
      message("Skipping station ", st, ": not DEEP and not SUNSUN")
      next
    }

    # Determine which transparency parameter to use
    station_params <- setdiff(
      names(station_data),
      c("STATIONID", "STATNAM", "year")
    )
    if (st %in% sunsun_all) {
      station_params <- station_params[station_params != "SECCHI_NVS"]
    } else {
      station_params <- station_params[station_params != "SECCHI"]
    }

    # --- Mann-Kendall for each parameter ---
    station_results <- list()
    for (param in station_params) {
      yrs <- sort(unique(station_data$year))
      if (length(yrs) < 10) {
        message(st, " ", param, " skipped: <10 total years")
        next
      }

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

      param_data <- station_data |> select(year, all_of(param)) |> arrange(year)
      temp <- na.omit(param_data[[param]])
      if (length(temp) < 5) {
        message(st, " ", param, " skipped: <5 non-NA values")
        next
      }

      mk <- mk.test(temp)
      sen <- sens.slope(temp)

      significant <- !is.na(mk$p.value) & mk$p.value < 0.05
      trend_cat <- dplyr::case_when(
        !significant ~ "Stable",
        param %in%
          c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
          mk$estimates[["tau"]] > 0 ~ "Worsening",
        param %in%
          c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
          mk$estimates[["tau"]] < 0 ~ "Improving",
        param %in%
          c("PH_epi", "SECCHI", "SECCHI_NVS") &
          mk$estimates[["tau"]] > 0 ~ "Improving",
        param %in%
          c("PH_epi", "SECCHI", "SECCHI_NVS") &
          mk$estimates[["tau"]] < 0 ~ "Worsening",
        TRUE ~ "Stable"
      )

      station_results[[length(station_results) + 1]] <- tibble(
        STATIONID = st,
        PARAMETER = param,
        n = length(temp),
        tau = mk$estimates[["tau"]],
        mk_p = mk$p.value,
        sen_slope = sen$estimates[["Sen's slope"]],
        sen_ci_lower = sen$conf.int[1],
        sen_ci_upper = sen$conf.int[2],
        TREND = trend_cat
      )
    }

    # Save individual station results
    if (length(station_results) > 0) {
      st_results <- bind_rows(station_results)
      write_csv(
        st_results,
        file.path(mk_path, paste0("MannKendall_", st, ".csv"))
      )
      results_list <- c(results_list, station_results)
    }
  }

  # Combine all results
  if (length(results_list) == 0) {
    message("No Mann-Kendall results generated.")
    return(invisible(NULL))
  }
  mk_summary <- bind_rows(results_list)

  # --- Recode parameter names ---
  mk_summary <- mk_summary |>
    mutate(
      PARAMETER = case_when(
        STATIONID %in% sunsun_all ~ dplyr::recode(
          PARAMETER,
          "SPCD_epi" = "Conductivity",
          "CHL_comp" = "Chlorophyll-a",
          "PH_epi" = "pH",
          "SECCHI" = "Transparency",
          "TP_epi" = "Phosphorus (Epilimnion)",
          "TP_hypo" = "Phosphorus (Hypolimnion)"
        ),
        TRUE ~ dplyr::recode(
          PARAMETER,
          "SPCD_epi" = "Conductivity (Epilimnion)",
          "CHL_comp" = "Chlorophyll-a (Composite)",
          "PH_epi" = "pH (Epilimnion)",
          "SECCHI_NVS" = "Transparency",
          "TP_epi" = "Phosphorus (Epilimnion)",
          "TP_hypo" = "Phosphorus (Hypolimnion)"
        )
      )
    )

  # --- Save individual station tables ---
  mk_summary |>
    group_by(STATIONID) |>
    group_split() |>
    walk(function(df) {
      st <- unique(df$STATIONID)
      write_csv(
        df |> select(PARAMETER, TREND),
        file.path(table_path, paste0("MK_TrendSummary_", st, ".csv"))
      )
    })

  # --- Combined SUNSUN NEARSHORE & TRIB summary tables ---
  mk_summary_nearshore <- mk_summary |>
    filter(STATIONID %in% sunsun_nearshore_stations) |>
    select(STATIONID, PARAMETER, TREND) |>
    arrange(STATIONID, PARAMETER)

  mk_summary_trib <- mk_summary |>
    filter(STATIONID %in% sunsun_trib_stations) |>
    select(STATIONID, PARAMETER, TREND) |>
    arrange(STATIONID, PARAMETER)

  write_csv(
    mk_summary_nearshore,
    file.path(table_path, "MK_TrendSummary_SUNSUN_Nearshore.csv")
  )
  write_csv(
    mk_summary_trib,
    file.path(table_path, "MK_TrendSummary_SUNSUN_Tribs.csv")
  )

  invisible(mk_summary)
}
