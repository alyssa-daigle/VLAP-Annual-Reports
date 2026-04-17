run_vlap_mannkendall <- function(
  data_year_median,
  MK_PATH = MK_PATH,
  TABLE_PATH = TABLE_PATH
) {
  # --- Create directories if needed ---
  if (!dir.exists(MK_PATH)) {
    dir.create(MK_PATH, recursive = TRUE)
  }
  if (!dir.exists(TABLE_PATH)) {
    dir.create(TABLE_PATH, recursive = TRUE)
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

      # Determine significance
      significant <- !is.na(mk$p.value) & mk$p.value < 0.05
      marginal <- !is.na(mk$p.value) & mk$p.value >= 0.05 & mk$p.value < 0.1
      tau <- mk$estimates[["tau"]]

      # Assign trend categories
      trend_cat <- dplyr::case_when(
        # --- Stable / not significant ---
        !significant & !marginal ~ "Stable",

        # --- Slightly trends: marginal p-value (0.05 <= p < 0.1) ---
        marginal &
          param %in% c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
          tau > 0 ~ "Slightly Worsening",
        marginal &
          param %in% c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
          tau < 0 ~ "Slightly Improving",
        marginal &
          param %in% c("PH_epi", "SECCHI", "SECCHI_NVS") &
          tau > 0 ~ "Slightly Improving",
        marginal &
          param %in% c("PH_epi", "SECCHI", "SECCHI_NVS") &
          tau < 0 ~ "Slightly Worsening",

        # --- Significant trends ---
        significant &
          param %in% c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
          tau > 0 ~ "Worsening",
        significant &
          param %in% c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
          tau < 0 ~ "Improving",
        significant &
          param %in% c("PH_epi", "SECCHI", "SECCHI_NVS") &
          tau > 0 ~ "Improving",
        significant &
          param %in% c("PH_epi", "SECCHI", "SECCHI_NVS") &
          tau < 0 ~ "Worsening",

        # --- Default fallback ---
        TRUE ~ "Stable"
      )

      station_results[[length(station_results) + 1]] <- tibble(
        STATIONID = st,
        Parameter = param,
        n = length(temp),
        tau = mk$estimates[["tau"]],
        mk_p = mk$p.value,
        sen_slope = sen$estimates[["Sen's slope"]],
        sen_ci_lower = sen$conf.int[1],
        sen_ci_upper = sen$conf.int[2],
        Trend = trend_cat
      )
    }

    # Save individual station results
    if (length(station_results) > 0) {
      st_results <- bind_rows(station_results)
      write_csv(
        st_results,
        file.path(MK_PATH, paste0("MannKendall_", st, ".csv"))
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
      Parameter = case_when(
        STATIONID %in% sunsun_all ~ dplyr::recode(
          Parameter,
          "SPCD_epi" = "Conductivity",
          "CHL_comp" = "Chlorophyll-a",
          "PH_epi" = "pH",
          "SECCHI" = "Transparency",
          "TP_epi" = "Phosphorus (Epilimnion)",
          "TP_hypo" = "Phosphorus (Hypolimnion)"
        ),
        TRUE ~ dplyr::recode(
          Parameter,
          "SPCD_epi" = "Conductivity (Epilimnion)",
          "CHL_comp" = "Chlorophyll-a (Composite)",
          "PH_epi" = "pH (Epilimnion)",
          "SECCHI_NVS" = "Transparency",
          "TP_epi" = "Phosphorus (Epilimnion)",
          "TP_hypo" = "Phosphorus (Hypolimnion)"
        )
      )
    )

  mk_summary |>
    dplyr::group_by(STATIONID) |>
    dplyr::group_split() |>
    purrr::walk(function(df) {
      st <- unique(df$STATIONID)

      readr::write_csv(
        df |> dplyr::select(Parameter, Trend),
        file.path(TABLE_PATH, paste0("MK_TrendSummary_", st, ".csv"))
      )
    })

  mk_summary_nearshore <- mk_summary |>
    dplyr::filter(STATIONID %in% sunsun_nearshore_stations)

  mk_summary_trib <- mk_summary |>
    dplyr::filter(STATIONID %in% sunsun_trib_stations)

  readr::write_csv(
    mk_summary_nearshore,
    file.path(TABLE_PATH, "MK_TrendSummary_SUNSUN_Nearshore.csv")
  )

  readr::write_csv(
    mk_summary_trib,
    file.path(TABLE_PATH, "MK_TrendSummary_SUNSUN_Tribs.csv")
  )

  invisible(mk_summary)
}
