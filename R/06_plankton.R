make_plankton <- function(INPUT_PATH, OUTPUT_PATH) {
  # -----------------------------
  # Ensure output directory exists
  # -----------------------------
  if (!dir.exists(OUTPUT_PATH)) {
    dir.create(OUTPUT_PATH, recursive = TRUE)
  }

  library(ggpattern)

  # -----------------------------
  # Load YEAR-driven plankton file
  # -----------------------------
  plankton_file <- file.path(
    INPUT_PATH,
    paste0("Historical_Phytoplankton_Data_Thru", YEAR, ".xlsx")
  )

  data <- read_excel(plankton_file)

  # -----------------------------
  # Processing
  # -----------------------------
  data <- data |>
    mutate(
      year = lubridate::year(`date-time`),
      month = lubridate::month(`date-time`)
    ) |>
    select(-`date-time`)

  # -----------------------------
  # Standardize COMMENTS
  # -----------------------------
  data <- data |>
    mutate(
      COMMENTS = as.character(COMMENTS),
      COMMENTS = trimws(COMMENTS),
      COMMENTS = toupper(COMMENTS),
      COMMENTS = ifelse(
        is.na(COMMENTS) | COMMENTS == "UNKNOWN PHYTO",
        "OTHER",
        COMMENTS
      )
    ) |>
    filter(
      COMMENTS %in%
        c(
          "GREEN",
          "GOLDEN-BROWN",
          "EUGLENOID",
          "DINOFLAGELLATE",
          "DIATOM",
          "CYANOBACTERIA",
          "CRYPTOMONAD",
          "XANTHOPHYTE",
          "OTHER"
        )
    )

  # -----------------------------
  # Relative abundance
  # -----------------------------
  rel_abund <- data |>
    group_by(WQDStationID, year, COMMENTS) |>
    summarise(
      total_count = sum(Percent, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(WQDStationID, year) |>
    mutate(
      rel_abundance = total_count / sum(total_count)
    ) |>
    ungroup()

  # -----------------------------
  # Lump low abundance groups
  # -----------------------------
  rel_abund <- rel_abund |>
    group_by(WQDStationID, year, COMMENTS) |>
    summarise(
      rel_abundance = sum(rel_abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(WQDStationID, year) |>
    mutate(
      COMMENTS = ifelse(
        rel_abundance < 0.03,
        "OTHER",
        COMMENTS
      )
    ) |>
    group_by(WQDStationID, year, COMMENTS) |>
    summarise(
      rel_abundance = sum(rel_abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    ungroup()

  data <- rel_abund

  # -----------------------------
  # Color palette
  # -----------------------------
  algae_colors <- c(
    "GREEN" = "#2E8B3A",
    "GOLDEN-BROWN" = "#D4A017",
    "EUGLENOID" = "#000000",
    "DINOFLAGELLATE" = "#CC79A7",
    "DIATOM" = "#0072B2",
    "CYANOBACTERIA" = "#E8601C",
    "CRYPTOMONAD" = "#56B4E9",
    "XANTHOPHYTE" = "#C2B280",
    "OTHER" = "grey70"
  )

  algae_labels <- c(
    "GREEN" = "Greens",
    "GOLDEN-BROWN" = "Golden-Browns",
    "EUGLENOID" = "Euglenoids",
    "DINOFLAGELLATE" = "Dinoflagellates",
    "DIATOM" = "Diatoms",
    "CYANOBACTERIA" = "Cyanobacteria",
    "CRYPTOMONAD" = "Cryptomonads",
    "XANTHOPHYTE" = "Xanthophytes",
    "OTHER" = "Other"
  )

  # -----------------------------
  # Pattern mappings
  # -----------------------------
  algae_patterns <- c(
    "GREEN" = "none",
    "GOLDEN-BROWN" = "stripe",
    "EUGLENOID" = "none",
    "DINOFLAGELLATE" = "stripe",
    "DIATOM" = "stripe",
    "CYANOBACTERIA" = "circle",
    "CRYPTOMONAD" = "crosshatch",
    "XANTHOPHYTE" = "crosshatch",
    "OTHER" = "stripe"
  )

  algae_pattern_angles <- c(
    "GREEN" = 30,
    "GOLDEN-BROWN" = 90,
    "EUGLENOID" = 90,
    "DINOFLAGELLATE" = 30,
    "DIATOM" = 0,
    "CYANOBACTERIA" = 30,
    "CRYPTOMONAD" = 0,
    "XANTHOPHYTE" = 120,
    "OTHER" = 120
  )

  # -----------------------------
  # Legend order
  # -----------------------------
  legend_order <- c(
    sort(setdiff(names(algae_colors), "OTHER")),
    "OTHER"
  )

  # -----------------------------
  # Get list of stations
  # -----------------------------
  stations <- sort(unique(data$WQDStationID))

  lapply(stations, function(WQDStationID) {
    message(paste0("Working on plankton for ", WQDStationID, "\n"))

    # -----------------------------
    # Subset station
    # -----------------------------
    plot_data <- data |>
      filter(.data$WQDStationID == .env$WQDStationID)

    # -----------------------------
    # Skip if no YEAR data
    # -----------------------------
    if (!any(plot_data$year >= YEAR, na.rm = TRUE)) {
      message("  -> Skipping ", WQDStationID, " (no ", YEAR, " data)\n")
      return(NULL)
    }

    # -----------------------------
    # Full year range
    # -----------------------------
    all_years <- seq(
      min(plot_data$year, na.rm = TRUE),
      max(plot_data$year, na.rm = TRUE),
      by = 1
    )

    # -----------------------------
    # Complete missing combinations
    # -----------------------------
    plot_data <- plot_data |>
      mutate(
        COMMENTS = factor(COMMENTS, levels = legend_order)
      ) |>
      tidyr::complete(
        year = all_years,
        COMMENTS = legend_order,
        fill = list(rel_abundance = 0)
      ) |>
      mutate(
        COMMENTS = factor(COMMENTS, levels = legend_order)
      )

    # -----------------------------
    # Main plot
    # -----------------------------
    p_main <- ggplot(
      plot_data,
      aes(
        x = factor(year),
        y = rel_abundance,
        fill = COMMENTS,
        pattern = COMMENTS,
        pattern_angle = COMMENTS
      )
    ) +

      geom_bar_pattern(
        stat = "identity",
        position = "stack",

        color = "white",
        linewidth = 0.05,

        pattern_fill = "grey25",
        pattern_colour = NA,
        pattern_density = 0.12,
        pattern_spacing = 0.04,
        pattern_key_scale_factor = 1.8
      ) +

      scale_y_continuous(
        labels = scales::percent,
        breaks = seq(0, 1, by = 0.1),
        expand = c(0, 0),
        limits = c(0, 1)
      ) +

      scale_fill_manual(
        values = algae_colors,
        labels = algae_labels,
        drop = FALSE
      ) +

      scale_pattern_manual(
        values = algae_patterns,
        drop = FALSE
      ) +

      scale_pattern_angle_manual(
        values = algae_pattern_angles,
        drop = FALSE
      ) +

      guides(
        fill = guide_legend(
          title = "Group",
          ncol = 1,
          byrow = TRUE,
          override.aes = {
            levs <- legend_order[legend_order %in% names(algae_colors)]

            list(
              pattern = algae_patterns[levs],
              pattern_angle = algae_pattern_angles[levs],
              pattern_fill = "grey25",
              pattern_colour = NA,
              pattern_density = 0.12,
              pattern_spacing = 0.01,
              pattern_key_scale_factor = 1.8
            )
          }
        ),

        pattern = "none",
        pattern_angle = "none"
      ) +

      labs(
        title = "Annual Phytoplankton Population",
        x = "Collection Year",
        y = "Relative Abundance",
        fill = NULL
      ) +

      theme_bw() +
      theme_plankton() +
      theme(
        legend.title = element_text(family = "Calibri", face = "bold")
      )

    # -----------------------------
    # Final plot
    # -----------------------------
    final_plot <- p_main

    # -----------------------------
    # Output filenames
    # -----------------------------
    filename <- paste0(
      WQDStationID,
      "_plankton.png"
    )

    output_file <- file.path(
      OUTPUT_PATH,
      filename
    )

    temp_file <- tempfile(
      pattern = "plankton_",
      tmpdir = OUTPUT_PATH,
      fileext = ".png"
    )

    border_file <- tempfile(
      pattern = "plankton_border_",
      tmpdir = OUTPUT_PATH,
      fileext = ".png"
    )

    # -----------------------------
    # Save plot to temporary file
    # -----------------------------
    ggsave(
      filename = temp_file,
      plot = final_plot,
      width = 8,
      height = 4,
      dpi = 300,
      bg = "white"
    )

    # -----------------------------
    # Add border to separate file
    # -----------------------------
    img <- magick::image_read(temp_file)

    img_bordered <- magick::image_border(
      img,
      color = "black",
      geometry = "7x7"
    )

    magick::image_write(
      img_bordered,
      path = border_file,
      format = "png"
    )

    # -----------------------------
    # Copy bordered image to final output
    # -----------------------------
    if (file.exists(output_file)) {
      file.remove(output_file)
    }

    file.copy(
      from = border_file,
      to = output_file,
      overwrite = TRUE
    )

    # -----------------------------
    # Clean up temporary files
    # -----------------------------
    unlink(
      c(
        temp_file,
        border_file
      )
    )
  })
}
