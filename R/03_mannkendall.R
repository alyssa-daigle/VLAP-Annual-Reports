run_vlap_mannkendall <- function(
  REG_MK,
  mk_path = "mannkendall",
  table_path = "tables_mk"
) {
  library(dplyr)
  library(tidyr)
  library(Kendall)
  library(trend)
  library(NADA)
  library(purrr)
  library(readr)

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
  stations_with_10yrs <- REG_MK |>
    group_by(stationid) |>
    summarise(
      consecutive_10yrs = has_10_consecutive_years(Year),
      .groups = "drop"
    )

  # save stations without 10 yrs
  write_csv(
    stations_with_10yrs |> filter(!consecutive_10yrs),
    file.path(table_path, "Stations_less_than_10yrs.csv")
  )

  # filter REG_MK to qualifying stations
  REG_MK_10yrs <- REG_MK |>
    inner_join(
      stations_with_10yrs |> filter(consecutive_10yrs),
      by = "stationid"
    )

  # loop through stations
  stations <- unique(REG_MK_10yrs$stationid)
  for (st in stations) {
    station_data <- REG_MK_10yrs |> filter(stationid == st)

    # reshape to long
    long_data <- station_data |>
      pivot_longer(
        cols = c(CHL_comp, SPCD_epi, PH_epi, TP_epi, TP_hypo, SECCHI),
        names_to = "parameter",
        values_to = "value"
      )

    # Mann-Kendall per parameter
    mk_summary <- long_data |>
      group_split(parameter) |>
      map_dfr(function(df) {
        param <- df$parameter[1]

        # drop NAs in value (and in censor column if censored)
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

        if (param %in% c("TP_epi", "TP_hypo")) {
          cen_col <- if (param == "TP_epi") "TP_cens_epi" else "TP_cens_hypo"
          df[[cen_col]] <- as.logical(df[[cen_col]])
          mk <- NADA::cenken(y = df$value, x = df$Year, ycen = df[[cen_col]])
          tau <- mk$tau
          slope <- mk$sen.slope
          pval <- NA_real_
        } else {
          mk <- Kendall::MannKendall(df$value)
          tau <- mk$tau
          pval <- mk$sl
          slope <- trend::sens.slope(df$value, df$Year)$estimates
        }

        slope_dir <- if (tau > 0) {
          "increasing"
        } else if (tau < 0) {
          "decreasing"
        } else {
          "stable"
        }

        # categorize the trends based on the parameter
        trend_cat <- case_when(
          param %in%
            c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "increasing" ~ "Worsening",
          param %in%
            c("CHL_comp", "TP_epi", "TP_hypo", "SPCD_epi") &
            slope_dir == "decreasing" ~ "Improving",
          param %in%
            c("PH_epi", "SECCHI") &
            slope_dir == "increasing" ~ "Improving",
          param %in%
            c("PH_epi", "SECCHI") &
            slope_dir == "decreasing" ~ "Worsening",
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
