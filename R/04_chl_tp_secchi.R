make_chl_tp_secchi <- function(data_plot, input_path, output_path) {
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  station_list <- data_plot |> distinct(STATIONID) |> pull(STATIONID)

  for (station_id in station_list) {
    message("Processing station: ", station_id)

    df_plot <- data_plot |>
      filter(STATIONID == station_id) |>
      filter(!is.na(CHL_comp) | !is.na(TP_epi) | !is.na(SECCHI_NVS)) |>
      arrange(year)

    if (nrow(df_plot) == 0 || !any(df_plot$year == 2025)) {
      warning("No data or missing 2025 for ", station_id, ", skipping...")
      next
    }

    all_years <- min(df_plot$year):max(df_plot$year)
    df_plot <- df_plot |>
      complete(
        year = all_years,
        fill = list(TP_epi = NA, CHL_comp = NA, SECCHI_NVS = NA)
      ) |>
      mutate(year = as.numeric(year)) |>
      arrange(year)

    y_max_left <- max(c(df_plot$TP_epi, df_plot$CHL_comp), na.rm = TRUE) * 1.5
    y_max_right <- max(df_plot$SECCHI_NVS, na.rm = TRUE) * 1.5

    mk_file <- paste0("mannkendall/MannKendall_", station_id, ".csv")
    has_MK <- file.exists(mk_file)
    MK_table <- if (has_MK) read.csv(mk_file) else NULL

    temp_path <- file.path(
      output_path,
      paste0(station_id, "_chl_tp_secchi.png")
    )

    # --- Create plot ---
    png(temp_path, width = 8, height = 4, units = "in", res = 120)
    par(family = "Calibri")
    par(mar = c(3.8, 4, 4, 3.8)) # reduce margins for more plotting area

    # Base plot x-axis limits (no automatic padding)
    n_years <- length(unique(df_plot$year))
    x_min <- min(df_plot$year)
    x_max <- max(df_plot$year)

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

    # Plot title
    title(
      main = "Historical Chlorophyll-a, Epilimnetic Phosphorus, and Transparency Data",
      line = 2.5,
      cex.main = 1.05
    )

    # Left y-axis
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

    # X-axis
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

    # secchi bars
    par(new = TRUE)
    plot(
      df_plot$year,
      df_plot$SECCHI_NVS,
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      ylim = c(y_max_right, 0),
      yaxs = "i"
    )

    # Dynamic bar width: narrower if fewer than 10 years
    bar_width <- if (n_years < 10) 0.18 else 0.28

    with(
      df_plot,
      rect(
        year - bar_width,
        0,
        year + bar_width,
        SECCHI_NVS,
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

    # Foreground TP & Chl-a
    par(new = TRUE)
    plot(
      df_plot$year,
      df_plot$TP_epi,
      type = "o",
      pch = 17,
      col = "red4",
      cex = 1.2,
      axes = FALSE,
      xlab = "",
      ylab = "",
      ylim = c(0, y_max_left),
      yaxs = "i",
      lwd = 2
    )
    lines(
      df_plot$year,
      df_plot$CHL_comp,
      type = "o",
      pch = 16,
      col = "green4",
      cex = 1.2,
      lwd = 2
    )

    # MK trend lines
    if (has_MK) {
      add_mk_line <- function(var, col) {
        slope <- MK_table |> filter(parameter == var) |> pull(sen_slope)
        if (length(slope) > 0 && !is.na(slope)) {
          df_var <- df_plot |> filter(!is.na(.data[[var]]))
          if (nrow(df_var) >= 2) {
            med_year <- median(df_var$year)
            med_val <- median(df_var[[var]])
            intercept <- med_val - slope * med_year
            x_vals <- range(df_var$year, na.rm = TRUE)
            y_vals <- intercept + slope * x_vals
            if (var == "SECCHI_NVS") {
              par(new = TRUE)
              plot(
                x_vals,
                y_vals,
                type = "l",
                col = "blue4",
                lty = 2,
                lwd = 1.5,
                axes = FALSE,
                xlab = "",
                ylab = "",
                ylim = c(y_max_right, 0),
                yaxs = "i"
              )
            } else {
              lines(x_vals, y_vals, col = col, lty = 2, lwd = 1.5)
            }
          }
        }
      }
      add_mk_line("TP_epi", "red4")
      add_mk_line("CHL_comp", "green4")
      add_mk_line("SECCHI_NVS", "blue4")
    }

    # --- Dynamic legend ---
    par(xpd = NA)

    # First legend: points
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

    # Second legend: trends
    trend_items <- c()
    col_items <- c()
    lty_items <- c()
    lwd_items <- c()

    if (has_MK) {
      # Transparency trend
      slope_sec <- MK_table |>
        filter(parameter == "SECCHI_NVS") |>
        pull(sen_slope)
      if (length(slope_sec) > 0 && !is.na(slope_sec)) {
        trend_items <- c(trend_items, "Transparency Trend")
        col_items <- c(col_items, "blue4")
        lty_items <- c(lty_items, 2)
        lwd_items <- c(lwd_items, 1.5)
      }

      # Chl trend
      slope_chl <- MK_table |>
        filter(parameter == "CHL_comp") |>
        pull(sen_slope)
      if (length(slope_chl) > 0 && !is.na(slope_chl)) {
        trend_items <- c(trend_items, "Chlorophyll-a Trend")
        col_items <- c(col_items, "green4")
        lty_items <- c(lty_items, 2)
        lwd_items <- c(lwd_items, 1.5)
      }

      # TP trend
      slope_tp <- MK_table |> filter(parameter == "TP_epi") |> pull(sen_slope)
      if (length(slope_tp) > 0 && !is.na(slope_tp)) {
        trend_items <- c(trend_items, "Total Phosphorus Trend")
        col_items <- c(col_items, "red4")
        lty_items <- c(lty_items, 2)
        lwd_items <- c(lwd_items, 1.5)
      }
    }

    if (length(trend_items) > 0) {
      legend(
        x = "top",
        inset = c(-0.18, -0.12), # adjust vertical spacing below first legend
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

    # Add black border via magick
    img <- magick::image_read(temp_path)
    img_bordered <- magick::image_border(img, color = "black", geometry = "3x3")
    magick::image_write(img_bordered, path = temp_path, format = "png")
  }

  message("All plots saved to: ", output_path)
}
