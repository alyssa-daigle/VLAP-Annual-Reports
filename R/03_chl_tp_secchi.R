make_chl_tp_secchi <- function(data_plot, input_path, output_path) {
  # Create output directory if it does not already exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Get list of unique stations to process
  station_list <- data_plot |>
    # only include stations with DEEP in STATNAM
    filter(grepl("DEEP", STATNAM, ignore.case = TRUE)) |>
    distinct(STATIONID) |>
    pull(STATIONID)

  # Loop through each station independently
  for (station_id in station_list) {
    message("Processing station: ", station_id)

    # Determine which Secchi variable to use
    secchi_var <- if (
      startsWith(station_id, "SUNSUN") |
        startsWith(station_id, "NUBNELD")
    ) {
      "SECCHI"
    } else {
      "SECCHI_NVS"
    }

    # Subset data to current station
    # Keep rows with at least one plotting variable present
    df_plot <- data_plot |>
      filter(STATIONID == station_id) |>
      filter(
        !is.na(CHL_comp) | !is.na(TP_epi) | !is.na(.data[[secchi_var]])
      ) |>
      arrange(year)

    # Skip station if no data or if 2025 is missing
    if (nrow(df_plot) == 0 || !any(df_plot$year == 2025)) {
      warning("No data or missing 2025 for ", station_id, ", skipping...")
      next
    }

    # Generate continuous sequence of years
    all_years <- min(df_plot$year):max(df_plot$year)

    fill_list <- list(
      TP_epi = NA,
      CHL_comp = NA
    )
    fill_list[[secchi_var]] <- NA

    df_plot <- df_plot |>
      complete(
        year = all_years,
        fill = fill_list
      ) |>
      mutate(year = as.numeric(year)) |>
      arrange(year)

    # Define y-axis limits
    y_max_left <- max(c(df_plot$TP_epi, df_plot$CHL_comp), na.rm = TRUE) * 1.5
    y_max_right <- max(df_plot[[secchi_var]], na.rm = TRUE) * 1.5

    if (!is.finite(y_max_left) || !is.finite(y_max_right)) {
      warning(
        "Skipping station ",
        station_id,
        ": no finite values for plotting"
      )
      next
    }

    # Mann Kendall
    mk_file <- paste0("mannkendall/MannKendall_", station_id, ".csv")
    has_MK <- file.exists(mk_file)
    MK_table <- if (has_MK) read.csv(mk_file) else NULL

    # Output path
    temp_path <- file.path(
      output_path,
      paste0(station_id, "_chl_tp_secchi.png")
    )

    # Begin plotting
    png(temp_path, width = 8, height = 4, units = "in", res = 200)
    par(family = "Calibri", mar = c(3.8, 4, 4, 3.8))

    n_years <- length(unique(df_plot$year))
    x_min <- min(df_plot$year)
    x_max <- max(df_plot$year)

    # Empty left-axis plot
    plot(
      df_plot$year,
      df_plot$TP_epi,
      type = "n",
      xlim = c(x_min, x_max),
      ylim = c(0, y_max_left),
      xlab = "",
      ylab = "",
      main = "",
      axes = FALSE,
      yaxs = "i"
    )
    box()
    title(
      main = "Historical Chlorophyll-a, Epilimnetic Phosphorus, and Transparency Data",
      line = 2.5,
      cex.main = 1.05
    )
    axis(
      side = 2,
      at = seq(0, ceiling(y_max_left / 5) * 5, by = 5),
      font.axis = 2,
      las = 1,
      cex.axis = 0.75
    )
    mtext(
      "Chlorophyll-a & Total Phosphorus (µg/L)",
      side = 2,
      line = 2.5,
      cex = 0.85,
      font = 2
    )

    axis(side = 1, at = df_plot$year, labels = FALSE)
    y_pos <- par("usr")[3] - 0.06 * diff(par("usr")[3:4])
    text(
      x = df_plot$year + 0.15,
      y = y_pos,
      labels = df_plot$year,
      srt = 45,
      adj = 1,
      xpd = TRUE,
      font = 2,
      cex = 0.65
    )
    mtext("Year", side = 1, line = 2, cex = 0.85, font = 2)

    # Secchi overlay
    par(new = TRUE)
    plot(
      df_plot$year,
      df_plot[[secchi_var]],
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      ylim = c(y_max_right, 0),
      yaxs = "i"
    )

    bar_width <- if (n_years < 10) 0.18 else 0.28
    with(
      df_plot,
      rect(
        year - bar_width,
        0,
        year + bar_width,
        df_plot[[secchi_var]],
        col = adjustcolor("lightsteelblue2", alpha.f = 0.5),
        border = "gray20"
      )
    )

    axis(
      side = 4,
      at = seq(0, ceiling(y_max_right), by = 1),
      labels = seq(0, ceiling(y_max_right), by = 1),
      font.axis = 2,
      cex.axis = 0.75,
      las = 2
    )
    mtext(
      "Transparency (m)",
      side = 4,
      line = 1.8,
      cex = 0.85,
      font = 2,
      las = 3
    )

    # TP and CHL overlay
    par(new = TRUE)
    plot(
      df_plot$year,
      df_plot$TP_epi,
      type = "o",
      pch = 17,
      col = "red4",
      cex = 1.0,
      axes = FALSE,
      xlab = "",
      ylab = "",
      ylim = c(0, y_max_left),
      yaxs = "i",
      lwd = 1.75
    )
    lines(
      df_plot$year,
      df_plot$CHL_comp,
      type = "o",
      pch = 16,
      col = "green4",
      cex = 1.0,
      lwd = 1.75
    )

    # Mann Kendall lines (if any)
    if (has_MK) {
      add_mk_line <- function(var, col, use_secchi = FALSE) {
        slope <- MK_table |> filter(PARAMETER == var) |> pull(sen_slope)
        if (length(slope) > 0 && !is.na(slope)) {
          df_var <- df_plot |> filter(!is.na(.data[[var]]))
          if (nrow(df_var) >= 2) {
            med_year <- median(df_var$year)
            med_val <- median(df_var[[var]])
            intercept <- med_val - slope * med_year
            x_vals <- range(df_var$year, na.rm = TRUE)
            y_vals <- intercept + slope * x_vals
            if (use_secchi) {
              par(new = TRUE)
              plot(
                x_vals,
                y_vals,
                type = "l",
                col = "blue4",
                lty = 2,
                lwd = 1.75,
                axes = FALSE,
                xlab = "",
                ylab = "",
                ylim = c(y_max_right, 0),
                yaxs = "i"
              )
            } else {
              lines(x_vals, y_vals, col = col, lty = 2, lwd = 1.75)
            }
          }
        }
      }

      add_mk_line("TP_epi", "red4")
      add_mk_line("CHL_comp", "green4")
      add_mk_line(secchi_var, "blue4", use_secchi = TRUE)
    }

    # Legends (unchanged)
    par(xpd = NA)
    legend(
      x = "top",
      inset = -0.18,
      legend = c(
        "Transparency (m)",
        "Chlorophyll a (µg/L)",
        "Total Phosphorus (µg/L)"
      ),
      pch = c(22, 16, 17),
      pt.bg = c("lightsteelblue2", NA, NA),
      col = c("black", "springgreen4", "red4"),
      lty = c(0, 1, 1),
      lwd = c(1, 1.1, 1.1),
      pt.cex = c(1.25, 0.8, 0.8),
      bty = "n",
      ncol = 3,
      cex = 0.6,
      text.font = 2
    )

    # Trend legend (unchanged except secchi_var)
    trend_items <- c()
    col_items <- c()
    lty_items <- c()
    lwd_items <- c()

    if (has_MK) {
      slope_sec <- MK_table |>
        filter(PARAMETER == secchi_var) |>
        pull(sen_slope)
      if (length(slope_sec) > 0 && !is.na(slope_sec)) {
        trend_items <- c(trend_items, "Transparency Trend")
        col_items <- c(col_items, "blue4")
        lty_items <- c(lty_items, 2)
        lwd_items <- c(lwd_items, 1.75)
      }
      slope_chl <- MK_table |>
        filter(PARAMETER == "CHL_comp") |>
        pull(sen_slope)
      if (length(slope_chl) > 0 && !is.na(slope_chl)) {
        trend_items <- c(trend_items, "Chlorophyll-a Trend")
        col_items <- c(col_items, "green4")
        lty_items <- c(lty_items, 2)
        lwd_items <- c(lwd_items, 1.75)
      }
      slope_tp <- MK_table |> filter(PARAMETER == "TP_epi") |> pull(sen_slope)
      if (length(slope_tp) > 0 && !is.na(slope_tp)) {
        trend_items <- c(trend_items, "Total Phosphorus Trend")
        col_items <- c(col_items, "red4")
        lty_items <- c(lty_items, 2)
        lwd_items <- c(lwd_items, 1.75)
      }
    }

    if (length(trend_items) > 0) {
      legend(
        x = "top",
        inset = c(-0.18, -0.12),
        legend = trend_items,
        col = col_items,
        lty = lty_items,
        lwd = lwd_items,
        bty = "n",
        ncol = 3,
        cex = 0.6,
        text.font = 2
      )
    }

    dev.off()

    # Add black border
    img <- magick::image_read(temp_path)
    img_bordered <- magick::image_border(
      img,
      color = "black",
      geometry = "3.5x3.5"
    )
    magick::image_write(img_bordered, path = temp_path, format = "png")
  }

  message("All plots saved to: ", output_path)
}
