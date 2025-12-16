make_plankton <- function(input_path, output_path) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(cowplot)
  library(magick)
  library(readxl)
  library(scales)

  # Load data
  data <- read_excel(paste0(input_path, "phytoplankton-master.xlsm"))

  # Ensure output directory exists
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Get list of stations
  stations <- sort(unique(data$stationid))

  # Loop over stations
  lapply(stations, function(station_id) {
    message(paste0("Working on plankton for ", station_id, "\n"))

    # Subset for the station
    plot_data <- data |>
      filter(stationid == station_id)

    # Skip if no data or missing year values
    if (nrow(plot_data) == 0 || all(is.na(plot_data$year))) {
      message("  -> Skipping ", station_id, " (no valid year data)\n")
      return(NULL)
    }

    # Get full year range (so gaps show up)
    all_years <- seq(
      min(plot_data$year, na.rm = TRUE),
      max(plot_data$year, na.rm = TRUE),
      by = 1
    )

    # Aggregate and ensure full grid of years × divisions
    plot_data <- plot_data |>
      group_by(year, division) |>
      summarise(
        total_cells = sum(count, na.rm = TRUE),
        .groups = "drop_last"
      ) |>
      mutate(
        rel_abund = total_cells / sum(total_cells),
        division = factor(division, levels = names(algae_colors))
      ) |>
      ungroup() |>
      complete(
        year = all_years,
        division = names(algae_colors),
        fill = list(total_cells = 0, rel_abund = 0)
      )

    # Main plot
    p_main <- ggplot(
      plot_data,
      aes(x = factor(year), y = rel_abund, fill = division)
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
    p_legend <- ggplot(plot_data, aes(x = 1, y = 1, fill = division)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(
        values = algae_colors,
        labels = algae_labels,
        breaks = rev(names(algae_labels)), # <-- reverse the order here
        drop = FALSE
      ) +
      labs(fill = NULL) +
      theme_void() +
      theme_plankton_legend() +
      guides(fill = guide_legend(ncol = 1))

    # Extract legend
    legend <- get_legend(p_legend)

    # Combine main plot and legend
    final_plot <- plot_grid(p_main, legend, rel_widths = c(6, 1.5))

    # Save plot and add full PNG border
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
