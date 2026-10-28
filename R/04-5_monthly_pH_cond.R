make_pH_conduc_monthly <- function(
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
    "monthly_ph_conduc"
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
        DEPTHZONE == "EPILIMNION",
        !is.na(NUMRESULT),
        !is.na(STARTDATE)
      ) |>
      mutate(
        SAMPLE_DATE = as.Date(STARTDATE),

        PARAMETER = case_when(
          # Match pH as a standalone term.
          # This prevents phosphorus from being
          # incorrectly classified as pH.

          grepl(
            "\\bpH\\b",
            WSHEDPARMNAME,
            ignore.case = TRUE
          ) ~ "PH",

          # Conductivity

          grepl(
            "CONDUCT",
            WSHEDPARMNAME,
            ignore.case = TRUE
          ) ~ "SPCD",

          TRUE ~ NA_character_
        )
      ) |>
      filter(
        !is.na(PARAMETER)
      ) |>
      arrange(
        SAMPLE_DATE
      )

    # -----------------------------------------------------
    # Skip if no usable data
    # -----------------------------------------------------

    if (nrow(df_plot) == 0) {
      warning(
        "No usable pH/conductivity data for ",
        station_id,
        ", skipping..."
      )

      next
    }

    # -----------------------------------------------------
    # Get unique sampling dates in chronological order
    # -----------------------------------------------------

    data_dates <- df_plot |>
      distinct(
        SAMPLE_DATE
      ) |>
      arrange(
        SAMPLE_DATE
      ) |>
      mutate(
        x = row_number()
      )

    n_dates <- nrow(data_dates)

    # -----------------------------------------------------
    # Skip stations with fewer than two sampling dates
    # -----------------------------------------------------

    if (n_dates < 2) {
      message(
        "Skipping ",
        station_id,
        ": only ",
        n_dates,
        " sampling date available"
      )

      next
    }

    # -----------------------------------------------------
    # Match each observation to its sampling-date position
    # -----------------------------------------------------

    df_plot <- df_plot |>
      left_join(
        data_dates,
        by = "SAMPLE_DATE"
      )

    # -----------------------------------------------------
    # Separate observations by parameter
    # -----------------------------------------------------

    ph_data <- df_plot |>
      filter(
        PARAMETER == "PH"
      ) |>
      arrange(
        SAMPLE_DATE
      )

    cond_data <- df_plot |>
      filter(
        PARAMETER == "SPCD"
      ) |>
      arrange(
        SAMPLE_DATE
      )

    # -----------------------------------------------------
    # Skip if either parameter is missing
    # -----------------------------------------------------

    if (
      nrow(ph_data) == 0 ||
        nrow(cond_data) == 0
    ) {
      message(
        "Skipping ",
        station_id,
        ": missing pH or conductivity data"
      )

      next
    }

    # -----------------------------------------------------
    # Calculate axis limits
    # -----------------------------------------------------

    min_pH <- min(
      ph_data$NUMRESULT,
      na.rm = TRUE
    )

    max_pH <- max(
      ph_data$NUMRESULT,
      na.rm = TRUE
    )

    max_cond <- max(
      cond_data$NUMRESULT,
      na.rm = TRUE
    )

    # Expand pH range to the nearest tenth, plus/minus 0.2

    min_pH <- floor(
      min_pH * 10
    ) /
      10 -
      0.2

    max_pH <- ceiling(
      max_pH * 10
    ) /
      10 +
      0.2

    # Conductivity axis maximum

    y_max_left <- max_cond * 1.2

    # Scale pH onto the conductivity plotting range

    scale_factor <- y_max_left / (max_pH - min_pH)

    # -----------------------------------------------------
    # Validate axis limits
    # -----------------------------------------------------

    if (
      !is.finite(min_pH) ||
        !is.finite(max_pH) ||
        !is.finite(y_max_left) ||
        max_pH <= min_pH ||
        y_max_left <= 0
    ) {
      message(
        "Skipping ",
        station_id,
        ": invalid axis limits"
      )

      next
    }

    # -----------------------------------------------------
    # Output filename
    # -----------------------------------------------------

    temp_path <- file.path(
      OUTPUT_PATH,
      paste0(
        station_id,
        "_pH_conduc_sampling_dates_2025.png"
      )
    )

    # -----------------------------------------------------
    # Start PNG
    #
    # Slightly wider rectangular dimensions
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
        6.0, # bottom, extra space for rotated date labels
        4.8, # left
        4.5, # top
        5.0 # right, extra room for pH axis label
      )
    )

    # -----------------------------------------------------
    # Initialize plot
    # -----------------------------------------------------

    plot(
      data_dates$x,
      rep(
        NA_real_,
        n_dates
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
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "",
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
    # Left axis: conductivity
    # -----------------------------------------------------

    axis(
      side = 2,
      font.axis = 2,
      las = 1,
      cex.axis = 1.20
    )

    mtext(
      "Conductivity (µS/cm)",
      side = 2,
      line = 3.0,
      cex = 1.30,
      font = 2
    )

    # -----------------------------------------------------
    # X-axis: chronological sampling dates
    #
    # Display up to 12 labels to prevent crowding.
    # All observations retain their individual date positions.
    # -----------------------------------------------------

    label_positions <- unique(
      round(
        seq(
          1,
          n_dates,
          length.out = min(
            n_dates,
            12
          )
        )
      )
    )

    axis(
      side = 1,
      at = label_positions,
      labels = FALSE,
      font.axis = 2,
      cex.axis = 1.20
    )

    # Sampling dates moved farther below the tick marks

    text(
      x = label_positions + 0.08,
      y = par("usr")[3] -
        0.075 * diff(par("usr")[3:4]),
      labels = format(
        data_dates$SAMPLE_DATE[label_positions],
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
    # Plot pH as bars
    # -----------------------------------------------------

    bar_width <- 0.22

    rect(
      xleft = ph_data$x - bar_width,
      ybottom = 0,
      xright = ph_data$x + bar_width,
      ytop = (ph_data$NUMRESULT - min_pH) * scale_factor,
      col = "lightgray",
      border = "black",
      lwd = 1
    )

    # -----------------------------------------------------
    # Plot conductivity as a line
    # -----------------------------------------------------

    lines(
      cond_data$x,
      cond_data$NUMRESULT,
      type = "o",
      pch = 21,
      bg = "red3",
      col = "red3",
      cex = 1.1,
      lwd = 1.75
    )

    # -----------------------------------------------------
    # Right axis: pH
    # -----------------------------------------------------

    pH_ticks <- pretty(
      c(
        min_pH,
        max_pH
      )
    )

    axis(
      side = 4,
      at = (pH_ticks - min_pH) * scale_factor,
      labels = pH_ticks,
      font.axis = 2,
      las = 2,
      cex.axis = 1.20
    )

    # Move the pH axis label farther to the right

    mtext(
      "pH",
      side = 4,
      line = 2.8,
      cex = 1.30,
      font = 2
    )

    # -----------------------------------------------------
    # Legend
    #
    # Larger symbols and reduced symbol-to-text spacing
    # -----------------------------------------------------

    par(
      xpd = NA
    )

    legend(
      x = "top",
      inset = -0.18,
      legend = c(
        "pH",
        "Conductivity (µS/cm)"
      ),
      pch = c(22, 21),
      pt.bg = c("lightgray", "red3"),
      col = c("black", "red3"),
      pt.cex = c(1.8),
      bty = "n",
      ncol = 2,
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
    "All pH/Conductivity plots saved to: ",
    OUTPUT_PATH
  )
}
