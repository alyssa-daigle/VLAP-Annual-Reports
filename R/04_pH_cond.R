make_pH_conduc <- function(data_plot, input_path, output_path) {
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  station_list <- data_plot |>
    filter(grepl("DEEP", STATNAM)) |>
    distinct(STATIONID) |>
    pull(STATIONID)

  for (station_id in station_list) {
    message("Processing station: ", station_id)

    df_plot <- data_plot |>
      filter(STATIONID == station_id) |>
      filter(!is.na(PH_epi) | !is.na(SPCD_epi)) |>
      arrange(year)

    if (nrow(df_plot) == 0 || !any(df_plot$year == 2025)) {
      next
    }

    all_years <- min(df_plot$year):max(df_plot$year)

    df_plot <- df_plot |>
      complete(
        year = all_years,
        fill = list(PH_epi = NA, SPCD_epi = NA)
      ) |>
      mutate(year = as.numeric(year)) |>
      arrange(year)

    # ---- Axis ranges ----
    min_pH <- min(df_plot$PH_epi, na.rm = TRUE)
    max_pH <- max(df_plot$PH_epi, na.rm = TRUE)
    max_cond <- max(df_plot$SPCD_epi, na.rm = TRUE)

    # Adjust axis ranges
    min_pH <- floor(min_pH * 10) / 10 - 0.2
    max_pH <- ceiling(max_pH * 10) / 10 + 0.2
    y_max_left <- max_cond * 1.2

    # Check before plotting
    if (!is.finite(min_pH) || !is.finite(max_pH) || !is.finite(y_max_left)) {
      message(
        "Skipping station ",
        station_id,
        ": no finite values for plotting"
      )
      next
    }

    scale_factor <- y_max_left / (max_pH - min_pH)

    # ---- MK table ----
    mk_file <- paste0("mannkendall/MannKendall_", station_id, ".csv")
    has_MK <- file.exists(mk_file)
    MK_table <- if (has_MK) read.csv(mk_file) else NULL

    temp_path <- file.path(
      output_path,
      paste0(station_id, "_pH_conduc.png")
    )

    # ---- Plot ----
    png(temp_path, width = 8, height = 4, units = "in", res = 200)
    par(family = "Calibri")
    par(mar = c(3.8, 4, 4, 3.8))

    x_min <- min(df_plot$year)
    x_max <- max(df_plot$year)

    # Base plot (conductivity scale)
    plot(
      df_plot$year,
      df_plot$SPCD_epi,
      type = "n",
      xlim = c(x_min, x_max),
      ylim = c(0, y_max_left),
      axes = FALSE,
      xlab = "",
      ylab = "",
      yaxs = "i"
    )
    box()

    title(
      main = "Historical Epilimnetic Conductivity and pH",
      line = 2.5,
      cex.main = 1.05
    )

    # Left axis (conductivity)
    axis(
      side = 2,
      font.axis = 2,
      las = 1,
      cex.axis = 0.75
    )
    mtext(
      "Conductivity (µS/cm)",
      side = 2,
      line = 2.5,
      cex = 0.85,
      font = 2
    )

    # X-axis
    axis(side = 1, at = df_plot$year, labels = FALSE)
    y_pos <- par("usr")[3] - 0.06 * diff(par("usr")[3:4])
    text(
      df_plot$year + 0.15,
      y_pos,
      labels = df_plot$year,
      srt = 45,
      adj = 1,
      xpd = TRUE,
      font = 2,
      cex = 0.65
    )
    mtext("Year", side = 1, line = 2, cex = 0.85, font = 2)

    # ---- pH bars (scaled to conductivity axis) ----
    bar_width <- if (length(all_years) < 10) 0.18 else 0.28

    with(
      df_plot,
      rect(
        year - bar_width,
        0,
        year + bar_width,
        (PH_epi - min_pH) * scale_factor,
        col = "lightgray", # <-- darker fill
        border = "black"
      )
    )

    # ---- Conductivity points & line ----
    lines(
      df_plot$year,
      df_plot$SPCD_epi,
      type = "o",
      pch = 21,
      bg = "red3",
      col = "red3",
      cex = 0.85,
      lwd = 1.75
    )

    # ---- Right axis (pH) ----
    axis(
      side = 4,
      at = (pretty(c(min_pH, max_pH)) - min_pH) * scale_factor,
      labels = pretty(c(min_pH, max_pH)),
      font.axis = 2,
      las = 1,
      cex.axis = 0.75
    )
    mtext(
      "pH",
      side = 4,
      line = 2.3,
      cex = 0.85,
      font = 2
    )

    # ---- MK trend lines ----
    if (has_MK) {
      add_mk_line <- function(var, col, is_ph = FALSE) {
        slope <- MK_table |> filter(PARAMETER == var) |> pull(sen_slope)

        if (length(slope) > 0 && !is.na(slope)) {
          df_var <- df_plot |> filter(!is.na(.data[[var]]))
          if (nrow(df_var) < 2) {
            return()
          }

          med_year <- median(df_var$year)
          med_val <- median(df_var[[var]])
          intercept <- med_val - slope * med_year

          x_vals <- range(df_var$year)

          if (is_ph) {
            y_vals <- (intercept + slope * x_vals - min_pH) * scale_factor
          } else {
            y_vals <- intercept + slope * x_vals
          }

          lines(x_vals, y_vals, col = col, lty = 2, lwd = 1.75)
        }
      }

      add_mk_line("SPCD_epi", "red3", is_ph = FALSE)
      add_mk_line("PH_epi", "black", is_ph = TRUE)
    }

    # --- Dynamic legend with trend numbers ---
    par(xpd = NA)

    # First legend: points
    legend(
      x = "top",
      inset = -0.18,
      legend = c("pH", "Conductivity"),
      pch = c(22, 21),
      pt.bg = c("lightgray", "red3"),
      col = c("black", "red3"),
      lty = c(0, 1),
      lwd = c(1, 1.5),
      pt.cex = c(1.0, 1.0),
      bty = "n",
      ncol = 2,
      cex = 0.65,
      text.font = 2,
      x.intersp = 0.3, # minimal space between items
      adj = 0
    )

    # Second legend: trends
    trend_items <- c()
    col_items <- c()
    lty_items <- c()
    lwd_items <- c()

    if (has_MK) {
      # pH trend
      slope_ph <- MK_table |> filter(PARAMETER == "PH_epi") |> pull(sen_slope)
      if (!is.na(slope_ph) & length(slope_ph) > 0) {
        trend_items <- c(trend_items, "pH Trend")
        col_items <- c(col_items, "black")
        lty_items <- c(lty_items, 2)
        lwd_items <- c(lwd_items, 1.75)
      }

      # Conductivity trend
      slope_cond <- MK_table |>
        filter(PARAMETER == "SPCD_epi") |>
        pull(sen_slope)
      if (!is.na(slope_cond) & length(slope_cond) > 0) {
        trend_items <- c(trend_items, "Conductivity Trend")
        col_items <- c(col_items, "red3")
        lty_items <- c(lty_items, 2)
        lwd_items <- c(lwd_items, 1.75)
      }
    }

    # Only add legend if there are trends
    if (length(trend_items) > 0) {
      legend(
        x = "top",
        inset = c(-0.18, -0.12),
        legend = trend_items,
        col = col_items,
        lty = lty_items,
        lwd = lwd_items,
        bty = "n",
        ncol = length(trend_items),
        cex = 0.65,
        text.font = 2,
        x.intersp = 0.3,
        adj = 0
      )
    }

    dev.off()

    # ---- Black border ----
    img <- magick::image_read(temp_path)
    img_bordered <- magick::image_border(
      img,
      color = "black",
      geometry = "3.5x3.5"
    )
    magick::image_write(img_bordered, temp_path)
  }

  message("All pH/Conductivity plots saved to: ", output_path)
}
