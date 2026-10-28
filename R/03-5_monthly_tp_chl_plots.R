make_chl_tp_secchi_monthly <- function(
  data_monthly,
  OUTPUT_BASE
) {
  # -------------------------------------------------------
  # Output path
  # -------------------------------------------------------

  OUTPUT_PATH <- file.path(
    OUTPUT_BASE,
    as.character(YEAR),
    "plots",
    "monthly_chl_tp_secchi"
  )

  dir.create(
    OUTPUT_PATH,
    recursive = TRUE,
    showWarnings = FALSE
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
        STARTDATE = if (inherits(STARTDATE, "Date")) {
          STARTDATE
        } else {
          as.Date(
            STARTDATE,
            format = "%d-%b-%y"
          )
        }
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
        !is.na(PARAMETER),
        !is.na(STARTDATE)
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
    # Get unique sampling dates
    #
    # Each date receives its own equally spaced position.
    # -----------------------------------------------------

    data_dates <- df_plot |>
      distinct(
        STARTDATE
      ) |>
      arrange(
        STARTDATE
      )

    # -----------------------------------------------------
    # Skip stations with only one sampling date
    # -----------------------------------------------------

    n_dates <- nrow(data_dates)

    if (n_dates < 2) {
      message(
        "Skipping ",
        station_id,
        ": only ",
        n_dates,
        " sampling date of data"
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
        1.30
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
        1.30
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
    #
    # Match pH/conductivity plot dimensions and resolution.
    # -----------------------------------------------------

    png(
      temp_path,
      width = 6.0,
      height = 4.5,
      units = "in",
      res = 250
    )

    # -----------------------------------------------------
    # Plot margins
    # -----------------------------------------------------

    par(
      family = "Calibri",
      mar = c(
        6.0, # bottom
        4.8, # left
        4.5, # top
        5.0 # right
      )
    )

    # -----------------------------------------------------
    # X positions
    # -----------------------------------------------------

    x_values <- seq_along(
      data_dates$STARTDATE
    )

    x_dates <- data_dates$STARTDATE

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
        n_dates + 0.5
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

    box(
      lwd = 1.4
    )

    # -----------------------------------------------------
    # Title
    # -----------------------------------------------------

    title(
      main = paste0(
        "Monthly Results ",
        unique(data_monthly$year)
      ),
      line = 2.8,
      cex.main = 1.65,
      font.main = 2
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
      cex.axis = 1.20
    )

    mtext(
      "Chlorophyll-a & Total Phosphorus (µg/L)",
      side = 2,
      line = 3.0,
      cex = 1.30,
      font = 2
    )

    # -----------------------------------------------------
    # X-axis: sampling dates
    #
    # Match pH/conductivity label font, angle, and position.
    # -----------------------------------------------------

    axis(
      side = 1,
      at = x_values,
      labels = FALSE,
      font.axis = 2,
      cex.axis = 1.20
    )

    text(
      x = x_values + 0.08,
      y = par("usr")[3] -
        0.075 * diff(par("usr")[3:4]),
      labels = format(
        x_dates,
        "%b %d"
      ),
      srt = 45,
      adj = 1,
      xpd = TRUE,
      font = 2,
      cex = 1.20
    )

    mtext(
      "Sampling Date",
      side = 1,
      line = 4.0,
      cex = 1.30,
      font = 2
    )

    # -----------------------------------------------------
    # Secchi overlay
    # -----------------------------------------------------

    par(
      new = TRUE
    )

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
        n_dates + 0.5
      ),
      ylim = c(
        y_max_right,
        0
      ),
      yaxs = "i"
    )

    # -----------------------------------------------------
    # Secchi bars
    # -----------------------------------------------------

    if (nrow(secchi_data) > 0) {
      secchi_plot <- secchi_data |>
        arrange(
          STARTDATE
        ) |>
        mutate(
          x = match(
            STARTDATE,
            x_dates
          )
        )

      bar_width <- 0.22

      rect(
        secchi_plot$x - bar_width,
        0,
        secchi_plot$x + bar_width,
        secchi_plot$NUMRESULT,
        col = adjustcolor(
          "lightsteelblue2",
          alpha.f = 0.5
        ),
        border = "gray20",
        lwd = 1
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
      cex.axis = 1.20,
      las = 2
    )

    mtext(
      "Transparency (m)",
      side = 4,
      line = 2.8,
      cex = 1.30,
      font = 2,
      las = 3
    )

    # -----------------------------------------------------
    # TP overlay
    # -----------------------------------------------------

    par(
      new = TRUE
    )

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
        n_dates + 0.5
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
        arrange(
          STARTDATE
        ) |>
        mutate(
          x = match(
            STARTDATE,
            x_dates
          )
        )

      lines(
        tp_plot$x,
        tp_plot$NUMRESULT,
        type = "o",
        pch = 17,
        col = "red4",
        cex = 2.0,
        lwd = 2.2
      )
    }

    # -----------------------------------------------------
    # Plot raw chlorophyll-a observations
    # -----------------------------------------------------

    if (nrow(chl_data) > 0) {
      chl_plot <- chl_data |>
        arrange(
          STARTDATE
        ) |>
        mutate(
          x = match(
            STARTDATE,
            x_dates
          )
        )

      lines(
        chl_plot$x,
        chl_plot$NUMRESULT,
        type = "o",
        pch = 16,
        col = "springgreen4",
        cex = 2.0,
        lwd = 2.2
      )
    }

    # -----------------------------------------------------
    # Legend
    #
    # Retains the existing three-item legend configuration.
    # -----------------------------------------------------

    par(
      xpd = NA
    )

    legend(
      x = "top",
      inset = -0.18,
      legend = c(
        "Chlor-a (µg/L)",
        "Total P (µg/L)",
        "Trans. (m)"
      ),
      pch = c(16, 17, 22),
      pt.bg = c(NA, NA, adjustcolor("lightsteelblue2", alpha.f = 0.5)),
      col = c("springgreen4", "red4", "gray20"),
      pt.cex = 1.8,
      bty = "n",
      ncol = 3,
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
