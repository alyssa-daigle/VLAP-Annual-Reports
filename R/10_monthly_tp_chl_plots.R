make_chl_tp_secchi_monthly <- function(
  data_monthly,
  OUTPUT_BASE
) {
  # -------------------------------------------------------
  # Output path
  # -------------------------------------------------------

  if (missing(OUTPUT_PATH) || is.null(OUTPUT_PATH)) {
    OUTPUT_BASE <- file.path(
      OUTPUT_BASE,
      "chl_tp_secchi_monthly"
    )
  }

  dir.create(
    OUTPUT_PATH,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # -------------------------------------------------------
  # Month order
  # -------------------------------------------------------

  month_levels <- c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  )

  # -------------------------------------------------------
  # Get list of unique DEEP stations
  # -------------------------------------------------------

  station_list <- data_monthly |>
    filter(
      year == 2025,
      grepl(
        "DEEP",
        STATNAM,
        ignore.case = TRUE
      )
    ) |>
    distinct(
      STATIONID
    ) |>
    pull(
      STATIONID
    )

  # -------------------------------------------------------
  # Loop through each station
  # -------------------------------------------------------

  for (station_id in station_list) {
    message(
      "Processing station: ",
      station_id
    )

    # -----------------------------------------------------
    # Get raw 2025 data for this station
    # -----------------------------------------------------

    df_plot <- data_monthly |>
      filter(
        year == 2025,
        STATIONID == station_id,
        (WSHEDPARMNAME %in%
          c(
            "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN",
            "PHOSPHORUS AS P"
          ) |
          (WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" &
            ANALYTICALMETHOD == "SECCHI-SCOPE")),
        !is.na(NUMRESULT)
      ) |>
      mutate(
        month_name = factor(
          month_name,
          levels = month_levels
        )
      )

    # -----------------------------------------------------
    # Identify which variable each observation represents
    # -----------------------------------------------------

    df_plot <- df_plot |>
      mutate(
        PARAMETER = case_when(
          WSHEDPARMNAME == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" ~
            "CHL",

          WSHEDPARMNAME == "PHOSPHORUS AS P" &
            DEPTHZONE == "EPILIMNION" ~
            "TP",

          WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" ~
            "SECCHI",

          TRUE ~ NA_character_
        )
      ) |>
      filter(
        !is.na(PARAMETER)
      )

    # -----------------------------------------------------
    # Skip if no usable data
    # -----------------------------------------------------

    if (nrow(df_plot) == 0) {
      warning(
        "No usable monthly data for ",
        station_id,
        ", skipping..."
      )

      next
    }

    # -----------------------------------------------------
    # Get only months that actually contain data
    # -----------------------------------------------------

    data_months <- df_plot |>
      distinct(
        month,
        month_name
      ) |>
      arrange(
        month
      )

    # -----------------------------------------------------
    # Skip stations with only one month of data
    # -----------------------------------------------------

    n_months <- nrow(data_months)

    if (n_months < 2) {
      message(
        "Skipping ",
        station_id,
        ": only ",
        n_months,
        " month of data"
      )

      next
    }

    # -----------------------------------------------------
    # Separate raw observations by parameter
    # -----------------------------------------------------

    chl_data <- df_plot |>
      filter(
        PARAMETER == "CHL"
      )

    tp_data <- df_plot |>
      filter(
        PARAMETER == "TP"
      )

    secchi_data <- df_plot |>
      filter(
        PARAMETER == "SECCHI"
      )

    # -----------------------------------------------------
    # Get maximum values for axes
    # -----------------------------------------------------

    left_values <- c(
      chl_data$NUMRESULT,
      tp_data$NUMRESULT
    )

    right_values <- secchi_data$NUMRESULT

    y_max_left <- if (
      length(left_values) > 0 &&
        any(is.finite(left_values))
    ) {
      max(
        left_values,
        na.rm = TRUE
      ) *
        1.5
    } else {
      1
    }

    y_max_right <- if (
      length(right_values) > 0 &&
        any(is.finite(right_values))
    ) {
      max(
        right_values,
        na.rm = TRUE
      ) *
        1.5
    } else {
      1
    }

    # -----------------------------------------------------
    # Output filename
    # -----------------------------------------------------

    temp_path <- file.path(
      OUTPUT_PATH,
      paste0(
        station_id,
        "_chl_tp_secchi_monthly_2025.png"
      )
    )

    # -----------------------------------------------------
    # Start PNG
    # -----------------------------------------------------

    png(
      temp_path,
      width = 8,
      height = 4,
      units = "in",
      res = 200
    )

    par(
      family = "Calibri",
      mar = c(
        3.8,
        4,
        4.2,
        3.8
      )
    )

    # -----------------------------------------------------
    # X positions
    #
    # One position for each month that actually has data
    # -----------------------------------------------------

    x_values <- seq_len(
      nrow(data_months)
    )

    # -----------------------------------------------------
    # Create lookup from actual month number to x position
    # -----------------------------------------------------

    month_lookup <- data_months |>
      mutate(
        x = x_values
      )

    # -----------------------------------------------------
    # Empty left-axis plot
    # -----------------------------------------------------

    plot(
      x_values,
      rep(
        NA_real_,
        length(x_values)
      ),
      type = "n",
      xlim = c(
        0.5,
        n_months + 0.5
      ),
      ylim = c(
        0,
        y_max_left
      ),
      xlab = "",
      ylab = "",
      main = "",
      axes = FALSE,
      yaxs = "i"
    )

    box()

    # -----------------------------------------------------
    # Title
    # -----------------------------------------------------

    title(
      main = "Monthly Chlorophyll-a, Epilimnetic Phosphorus, and Transparency Data",
      line = 2.5,
      cex.main = 1.25
    )

    # -----------------------------------------------------
    # Left axis
    # -----------------------------------------------------

    axis(
      side = 2,
      at = seq(
        0,
        ceiling(y_max_left / 5) * 5,
        by = 5
      ),
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

    # -----------------------------------------------------
    # X-axis labels
    # Only months that actually have data
    # -----------------------------------------------------

    axis(
      side = 1,
      at = x_values,
      labels = FALSE
    )

    text(
      x = x_values,
      y = par("usr")[3] -
        0.06 *
          diff(par("usr")[3:4]),
      labels = month.abb[
        data_months$month
      ],
      srt = 45,
      adj = 1,
      xpd = TRUE,
      font = 2,
      cex = 0.65
    )

    mtext(
      "Month",
      side = 1,
      line = 2,
      cex = 0.85,
      font = 2
    )

    # -----------------------------------------------------
    # Secchi overlay
    # -----------------------------------------------------

    par(new = TRUE)

    plot(
      x_values,
      rep(
        NA_real_,
        length(x_values)
      ),
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      xlim = c(
        0.5,
        n_months + 0.5
      ),
      ylim = c(
        y_max_right,
        0
      ),
      yaxs = "i"
    )

    # -----------------------------------------------------
    # Secchi bars
    #
    # Only draw bars for months with Secchi data
    # -----------------------------------------------------

    if (nrow(secchi_data) > 0) {
      secchi_plot <- secchi_data |>
        left_join(
          month_lookup,
          by = c(
            "month",
            "month_name"
          )
        )

      bar_width <- 0.2

      rect(
        secchi_plot$x - bar_width,
        0,
        secchi_plot$x + bar_width,
        secchi_plot$NUMRESULT,
        col = adjustcolor(
          "lightsteelblue2",
          alpha.f = 0.5
        ),
        border = "gray20"
      )
    }

    # -----------------------------------------------------
    # Right axis
    # -----------------------------------------------------

    axis(
      side = 4,
      at = seq(
        0,
        ceiling(y_max_right),
        by = 1
      ),
      labels = seq(
        0,
        ceiling(y_max_right),
        by = 1
      ),
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

    # -----------------------------------------------------
    # TP overlay
    # -----------------------------------------------------

    par(new = TRUE)

    plot(
      x_values,
      rep(
        NA_real_,
        length(x_values)
      ),
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      xlim = c(
        0.5,
        n_months + 0.5
      ),
      ylim = c(
        0,
        y_max_left
      ),
      yaxs = "i"
    )

    # -----------------------------------------------------
    # Plot raw TP observations
    # -----------------------------------------------------

    if (nrow(tp_data) > 0) {
      tp_plot <- tp_data |>
        left_join(
          month_lookup,
          by = c(
            "month",
            "month_name"
          )
        ) |>
        arrange(
          month
        )

      lines(
        tp_plot$x,
        tp_plot$NUMRESULT,
        type = "o",
        pch = 17,
        col = "red4",
        cex = 1.5,
        lwd = 1.75
      )
    }

    # -----------------------------------------------------
    # Plot raw chlorophyll-a observations
    # -----------------------------------------------------

    if (nrow(chl_data) > 0) {
      chl_plot <- chl_data |>
        left_join(
          month_lookup,
          by = c(
            "month",
            "month_name"
          )
        ) |>
        arrange(
          month
        )

      lines(
        chl_plot$x,
        chl_plot$NUMRESULT,
        type = "o",
        pch = 16,
        col = "springgreen4",
        cex = 1.5,
        lwd = 1.75
      )
    }

    # -----------------------------------------------------
    # Legend
    # -----------------------------------------------------

    par(
      xpd = NA
    )

    legend(
      x = "top",
      inset = -0.18,
      legend = c(
        "Transparency (m)",
        "Chlorophyll-a (µg/L)",
        "Total Phosphorus (µg/L)"
      ),
      pch = c(
        22,
        16,
        17
      ),
      pt.bg = c(
        "lightsteelblue2",
        NA,
        NA
      ),
      col = c(
        "black",
        "springgreen4",
        "red4"
      ),
      lty = c(
        0,
        1,
        1
      ),
      lwd = c(
        1,
        1.1,
        1.1
      ),
      pt.cex = c(
        1.75,
        1.25,
        1.25
      ),
      bty = "n",
      ncol = 3,
      cex = 0.85,
      text.font = 2
    )

    # -----------------------------------------------------
    # Close PNG
    # -----------------------------------------------------

    dev.off()

    # -----------------------------------------------------
    # Add black border
    # -----------------------------------------------------

    img <- magick::image_read(
      temp_path
    )

    img_bordered <- magick::image_border(
      img,
      color = "black",
      geometry = "3.5x3.5"
    )

    magick::image_write(
      img_bordered,
      path = temp_path,
      format = "png"
    )
  }

  # -------------------------------------------------------
  # Finished
  # -------------------------------------------------------

  message(
    "All monthly plots saved to: ",
    OUTPUT_PATH
  )
}
