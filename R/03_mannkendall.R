run_vlap_mannkendall <- function(
  REG_MK,
  mk_path = "mannkendall",
  table_path = "tables"
) {
  # ---- Create output directories ----
  if (!dir.exists(mk_path)) {
    dir.create(mk_path, recursive = TRUE)
  }

  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # ---- Add year column (eligibility checks only) ----
  REG_MK <- REG_MK |>
    mutate(year = year(date))

  current_year <- 2025

  # Alphabetically sorted station list
  stations <- sort(unique(REG_MK$stationid))

  # Explicitly exclude non-parameter fields
  parameters <- setdiff(
    names(REG_MK),
    c("stationid", "date", "year")
  )

  results_list <- list()

  # ---- Loop through stations ----
  for (st in stations) {
    station_data <- REG_MK |> filter(stationid == st)
    yrs <- sort(unique(station_data$year))

    # ---- Eligibility checks ----
    has_current_year <- current_year %in% yrs

    consec_lengths <- rle(diff(yrs) == 1)
    max_consec <- if (length(consec_lengths$lengths) > 0) {
      max(consec_lengths$lengths[consec_lengths$values == TRUE]) + 1
    } else {
      1
    }

    if (has_current_year & max_consec >= 10) {
      for (param in parameters) {
        param_data <- station_data |>
          select(date, all_of(param)) |>
          arrange(date) |>
          mutate(temp = as.numeric(.data[[param]])) |>
          drop_na(temp)

        # Minimum data points for MK
        if (nrow(param_data) >= 5) {
          mk <- mk.test(param_data$temp)
          sen <- sens.slope(param_data$temp)

          # ---- Trend classification ----
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
            stationid = st,
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
      message(
        st,
        " skipped: ",
        if (!has_current_year) "no current-year data; " else "",
        if (max_consec < 10) "less than 10 consecutive years" else ""
      )
    }
  }

  # ---- Combine results ----
  mk_summary <- bind_rows(results_list)

  # ---- Write one full MK CSV per station ----
  mk_summary |>
    group_by(stationid) |>
    group_split() |>
    walk(function(df) {
      st <- unique(df$stationid)
      write_csv(
        df,
        file.path(mk_path, paste0("MannKendall_", st, ".csv"))
      )
    })

  # ---- Write one display table per station ----
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
    select(stationid, PARAMETER, TREND = trend) |>
    group_by(stationid) |>
    group_split() |>
    walk(function(df) {
      st <- unique(df$stationid)
      write_csv(
        df |> select(PARAMETER, TREND),
        file.path(table_path, paste0("MK_TrendSummary_", st, ".csv"))
      )
    })

  invisible(mk_summary)
}


run_vlap_mannkendall(REG_MK, mk_path, table_path)
