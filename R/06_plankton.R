make_plankton <- function(output_path) {
  # Ensure output directory exists
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # -----------------------------
  # plankton data processing
  # -----------------------------
  data <- read_excel(paste0(
    input_path,
    "Historical_Phytoplankton_Data_Thru2025.xlsm"
  ))

  data <- data |>
    mutate(
      year = year(date),
      month = month(date)
    ) |>
    select(-date)

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

  data <- rel_abund

  # -----------------------------
  # GLOBAL CHECK FOR 2024+ DATA
  # -----------------------------
  if (!any(data$year >= 2024, na.rm = TRUE)) {
    message("No plankton data from 2024 or newer. No reports generated.")
    return(invisible(NULL))
  }

  # Get list of stations
  stations <- sort(unique(data$stationID))

  lapply(stations, function(station_id) {
    message(paste0("Working on plankton for ", station_id, "\n"))

    # Subset for station
    plot_data <- data |>
      filter(stationID == station_id)

    # -----------------------------
    # STATION-LEVEL CHECK FOR 2024+
    # -----------------------------
    if (!any(plot_data$year >= 2024, na.rm = TRUE)) {
      message("  -> Skipping ", station_id, " (no 2024+ data)\n")
      return(NULL)
    }

    # Full year range
    all_years <- seq(
      min(plot_data$year, na.rm = TRUE),
      max(plot_data$year, na.rm = TRUE),
      by = 1
    )

    plot_data <- plot_data |>
      mutate(
        group = factor(group, levels = names(algae_colors))
      ) |>
      complete(
        year = all_years,
        group = names(algae_colors),
        fill = list(rel_abundance = 0)
      )

    # Main stacked bar plot
    p_main <- ggplot(
      plot_data,
      aes(x = factor(year), y = rel_abundance, fill = group)
    ) +
      geom_bar(stat = "identity") +
      scale_y_continuous(
        labels = scales::percent,
        breaks = seq(0, 1, by = 0.1),
        expand = c(0, 0),
        limits = c(0, 1)
      ) +
      scale_fill_manual(values = algae_colors, drop = FALSE) +
      labs(
        title = "Annual Phytoplankton Population",
        x = "Collection Year",
        y = "Relative Percent of Taxa",
        fill = ""
      ) +
      theme_bw() +
      theme_plankton()

    # Legend-only plot
    p_legend <- ggplot(plot_data, aes(x = 1, y = 1, fill = group)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(
        values = algae_colors,
        labels = algae_labels,
        breaks = rev(names(algae_labels)),
        drop = FALSE
      ) +
      labs(fill = NULL) +
      theme_void() +
      theme_plankton_legend() +
      guides(fill = guide_legend(ncol = 1))

    legend <- get_legend(p_legend)

    final_plot <- plot_grid(p_main, legend, rel_widths = c(6, 1.5))

    filename <- paste0(station_id, "_plankton.png")
    temp_path <- file.path(output_path, filename)

    ggsave(
      temp_path,
      plot = final_plot,
      width = 8,
      height = 4,
      dpi = 300,
      bg = "white"
    )

    img <- magick::image_read(temp_path)
    img_bordered <- magick::image_border(
      img,
      color = "black",
      geometry = "7x7"
    )
    magick::image_write(img_bordered, path = temp_path, format = "png")
  })
}
