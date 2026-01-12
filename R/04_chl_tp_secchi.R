make_chl_tp_secchi_test <- function(input_path, output_path) {
  library(dplyr)
  library(tidyr)

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  station_list <- REG_plot |> distinct(stationid) |> pull(stationid)

  for (station_id in station_list) {
    message("Processing station: ", station_id)

    df_plot <- REG_plot |>
      filter(stationid == station_id) |>
      filter(!is.na(CHL_comp) | !is.na(TP_epi) | !is.na(SECCHI)) |>
      arrange(Year)

    if (nrow(df_plot) == 0 || !any(df_plot$Year == 2025)) {
      warning("No data or missing 2025 for ", station_id, ", skipping...")
      next
    }

    all_years <- min(df_plot$Year):max(df_plot$Year)
    df_plot <- df_plot |>
      complete(
        Year = all_years,
        fill = list(TP_epi = NA, CHL_comp = NA, SECCHI = NA)
      ) |>
      mutate(Year = as.numeric(Year)) |>
      arrange(Year)

    y_max_left <- max(c(df_plot$TP_epi, df_plot$CHL_comp), na.rm = TRUE) * 1.5
    y_max_right <- max(df_plot$SECCHI, na.rm = TRUE) * 1.5

    mk_file <- paste0("mannkendall/MannKendall_", station_id, ".csv")
    has_MK <- file.exists(mk_file)
    MK_table <- if (has_MK) read.csv(mk_file) else NULL

    png_filename <- file.path(
      output_path,
      paste0(station_id, "_chl_tp_secchi.png")
    )
    png(png_filename, width = 10, height = 6, units = "in", res = 120)

    # Symmetric margins
    par(mar = c(5, 5, 5, 5) + 0.1)

    # --- Base plot ---
    plot(
      df_plot$Year,
      df_plot$TP_epi,
      type = "n",
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
      line = 3
    )

    # Left y-axis
    y_left_breaks <- seq(0, ceiling(y_max_left / 5) * 5, by = 5)
    axis(side = 2, at = y_left_breaks, font.axis = 2, las = 1, cex.axis = 0.95)
    mtext(
      "Chlorophyll-a & Total Phosphorus (µg/L)",
      side = 2,
      line = 3,
      cex = 0.95,
      font = 2
    )

    # X-axis ticks (no labels yet)
    axis(side = 1, at = df_plot$Year, labels = FALSE)

    # X-axis labels rotated, slightly below ticks
    y_pos <- par("usr")[3] - 0.045 * diff(par("usr")[3:4])
    text(
      x = df_plot$Year + 0.15,
      y = y_pos,
      labels = df_plot$Year,
      srt = 45,
      adj = 1,
      xpd = TRUE,
      font = 2,
      cex = 0.85
    )

    # X-axis label
    mtext("Year", side = 1, line = 3, cex = 0.95, font = 2)

    # --- Secchi bars ---
    par(new = TRUE)
    plot(
      df_plot$Year,
      df_plot$SECCHI,
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      ylim = c(y_max_right, 0),
      yaxs = "i"
    )
    bar_half_width <- 0.35
    secchi_col <- adjustcolor("lightsteelblue2", alpha.f = 0.5)
    with(
      df_plot,
      rect(
        Year - bar_half_width,
        0,
        Year + bar_half_width,
        SECCHI,
        col = secchi_col,
        border = "gray20"
      )
    )

    # Right y-axis
    y_right_breaks <- seq(0, ceiling(y_max_right), by = 1)
    axis(
      side = 4,
      at = y_right_breaks,
      labels = y_right_breaks,
      font.axis = 2,
      cex.axis = 0.95,
      las = 2
    )

    # Right y-axis title
    usr <- par("usr")
    par(xpd = NA)
    # Right y-axis title using mtext()
    mtext(
      "Transparency (m)", # label
      side = 4, # right side
      line = 2.5, # distance from axis (adjust as needed)
      cex = 0.95, # font size
      font = 2,
      las = 3
    )

    # --- Foreground TP & Chl-a ---
    par(new = TRUE)
    plot(
      df_plot$Year,
      df_plot$TP_epi,
      type = "b",
      pch = 17,
      col = "red4",
      axes = FALSE,
      xlab = "",
      ylab = "",
      ylim = c(0, y_max_left),
      yaxs = "i"
    )
    lines(df_plot$Year, df_plot$CHL_comp, type = "b", pch = 16, col = "green4")

    # --- MK trend lines ---
    if (has_MK) {
      # TP & Chl-a trend lines (left y-axis)
      add_mk_line <- function(var, col, ylim) {
        slope <- MK_table |> filter(parameter == var) |> pull(slope)
        if (length(slope) == 0 || is.na(slope)) {
          return()
        }
        df_var <- df_plot |> filter(!is.na(.data[[var]]))
        if (nrow(df_var) < 2) {
          return()
        }
        med_year <- median(df_var$Year)
        med_val <- median(df_var[[var]])
        intercept <- med_val - slope * med_year
        x_vals <- range(df_var$Year, na.rm = TRUE)
        y_vals <- intercept + slope * x_vals
        lines(x_vals, y_vals, col = col, lty = 2, lwd = 1.5)
      }
      add_mk_line("TP_epi", "red4", c(0, y_max_left))
      add_mk_line("CHL_comp", "green4", c(0, y_max_left))

      # Secchi trend line on right y-axis
      secchi_slope <- MK_table |> filter(parameter == "SECCHI") |> pull(slope)
      if (length(secchi_slope) > 0 && !is.na(secchi_slope)) {
        df_secchi <- df_plot |> filter(!is.na(SECCHI))
        if (nrow(df_secchi) >= 2) {
          med_year <- median(df_secchi$Year)
          med_val <- median(df_secchi$SECCHI)
          intercept <- med_val - secchi_slope * med_year
          x_vals <- range(df_secchi$Year, na.rm = TRUE)
          y_vals <- intercept + secchi_slope * x_vals

          # Plot on right axis
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
        }
      }
    }

    # --- Legend ---
    par(xpd = NA)
    legend(
      "top",
      inset = -0.13,
      legend = c(
        "Transparency (m)",
        "Trans. Trend",
        "Chlorophyll a (µg/L)",
        "Chl-a Trend",
        "Phosphorus (µg/L)",
        "Phos. Trend"
      ),
      pch = c(22, NA, 16, NA, 17, NA),
      pt.bg = c("lightsteelblue2", NA, NA, NA, NA, NA),
      col = c("black", "blue4", "springgreen4", "green4", "red4", "red4"),
      lty = c(0, 2, 1, 2, 1, 2),
      lwd = c(1, 1.5, 1.1, 1.5, 1.1, 1.5),
      pt.cex = c(1.25, NA, 0.8, NA, 0.8, NA),
      bty = "n",
      ncol = 3,
      cex = 0.6,
      text.font = 2
    )

    dev.off()
  }

  message("All plots saved to: ", output_path)
}
