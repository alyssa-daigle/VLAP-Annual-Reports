make_plankton <- function(INPUT_PATH, OUTPUT_PATH) {
  # -----------------------------
  # Ensure output directory exists
  # -----------------------------
  if (!dir.exists(OUTPUT_PATH)) {
    dir.create(OUTPUT_PATH, recursive = TRUE)
  }

  # -----------------------------
  # Load YEAR-driven plankton file
  # -----------------------------
  plankton_file <- file.path(
    INPUT_PATH,
    paste0("Historical_Phytoplankton_Data_Thru", YEAR, ".xlsm")
  )

  data <- read_excel(plankton_file)

  # -----------------------------
  # Processing
  # -----------------------------
  data <- data |>
    mutate(
      year = year(date),
      month = month(date)
    ) |>
    select(-date)

  data <- data |>
    mutate(
      group = as.character(group),
      group = ifelse(is.na(group) | group == "UNKNOWN PHYTO", "OTHER", group)
    )

  rel_abund <- data |>
    group_by(stationID, year, group) |>
    summarise(
      total_count = sum(count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(stationID, year) |>
    mutate(
      rel_abundance = total_count / sum(total_count)
    ) |>
    ungroup()

  rel_abund <- rel_abund |>
    group_by(stationID, year, group) |>
    summarise(
      rel_abundance = sum(rel_abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(stationID, year) |>
    mutate(
      group = ifelse(rel_abundance < 0.03, "OTHER", group)
    ) |>
    group_by(stationID, year, group) |>
    summarise(
      rel_abundance = sum(rel_abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    ungroup()

  data <- rel_abund

  # Get list of stations
  stations <- sort(unique(data$stationID))

  lapply(stations, function(station_id) {
    message(paste0("Working on plankton for ", station_id, "\n"))

    # Subset for station
    plot_data <- data |>
      filter(stationID == station_id)

    # -----------------------------
    # STATION-LEVEL CHECK FOR YEAR+
    # -----------------------------
    if (!any(plot_data$year >= YEAR, na.rm = TRUE)) {
      message("  -> Skipping ", station_id, " (no ", YEAR, " data)\n")
      return(NULL)
    }
    # Full year range
    all_years <- seq(
      min(plot_data$year, na.rm = TRUE),
      max(plot_data$year, na.rm = TRUE),
      by = 1
    )

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

    legend_order <- c(
      sort(setdiff(names(algae_colors), "OTHER")),
      "OTHER"
    )

    plot_data <- plot_data |>
      mutate(group = factor(group, levels = legend_order)) |>
      tidyr::complete(
        year = all_years,
        group = legend_order,
        fill = list(rel_abundance = 0)
      )

    present_groups <- plot_data |>
      group_by(group) |>
      summarise(total = sum(rel_abundance), .groups = "drop") |>
      filter(total > 0) |>
      pull(group)

    # -----------------------------
    # MAIN PLOT (no patterns)
    # -----------------------------

    #plankton color palette and renaming

    p_main <- ggplot(
      plot_data,
      aes(
        x = factor(year),
        y = rel_abundance,
        fill = group
      )
    ) +
      geom_bar(
        stat = "identity",
        color = "white",
        linewidth = 0.05
      ) +
      scale_y_continuous(
        labels = scales::percent,
        breaks = seq(0, 1, by = 0.1),
        expand = c(0, 0),
        limits = c(0, 1)
      ) +
      scale_fill_manual(
        values = algae_colors[present_groups],
        labels = algae_labels[present_groups],
        breaks = present_groups,
        drop = FALSE
      ) +
      labs(
        title = "Annual Phytoplankton Population",
        x = "Collection Year",
        y = "Relative Abundance",
        fill = NULL
      ) +
      theme_bw() +
      theme_plankton()

    # -----------------------------
    # LEGEND-only plot (no patterns)
    # -----------------------------
    p_legend <- ggplot(
      plot_data,
      aes(x = 1, y = 1, fill = group)
    ) +
      geom_bar(
        stat = "identity",
        color = "white",
        linewidth = 0.05
      ) +
      scale_fill_manual(
        values = algae_colors[present_groups],
        labels = algae_labels[present_groups],
        breaks = present_groups,
        drop = FALSE
      ) +
      labs(fill = NULL) +
      theme_void() +
      theme_plankton_legend() +
      guides(
        fill = guide_legend(ncol = 1)
      )

    legend <- get_legend(p_legend)

    # -----------------------------
    # Combine + SAVE
    # -----------------------------
    final_plot <- plot_grid(p_main, legend, rel_widths = c(6, 1.5))

    filename <- paste0(station_id, "_plankton.png")
    temp_path <- file.path(OUTPUT_PATH, filename)

    ggsave(
      temp_path,
      plot = final_plot,
      width = 8,
      height = 4,
      dpi = 300,
      bg = "white"
    )

    # Add border
    img <- magick::image_read(temp_path)
    img_bordered <- magick::image_border(
      img,
      color = "black",
      geometry = "7x7"
    )
    magick::image_write(img_bordered, path = temp_path, format = "png")
  })
}
