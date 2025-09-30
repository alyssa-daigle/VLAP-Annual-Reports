make_plankton <- function(input_path, output_path) {
  # Load data
  data <- read_excel(paste0(input_path, "VLAPPlanktonGraphicYear-2024.xlsx"))

  # If output directory doesn't exist, make it
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Define color palette
  algae_colors <- c(
    "UNKNOWN" = "#772c2a",
    "GREENS" = "#2c4d75",
    "GOLDEN-BROWN" = "#4bacc6",
    "EUGLENOIDS" = "#9bbb59",
    "DINOFLAGELLATES" = "#e46c0a",
    "DIATOMS" = "#7f7f7f",
    "CYANOBACTERIA" = "#604a7b",
    "CRYPTOMONADS" = "#4f6228"
  )

  # Get list of stations
  stations <- unique(data$WQDStationID)

  # Loop over stations
  lapply(stations, function(station_id) {
    # Prepare data for this station
    plot_data <- data |>
      filter(WQDStationID == station_id) |>
      group_by(Year, MONTH, COMMENTS) |>
      summarise(
        total_cells = sum(`Cell Count`, na.rm = TRUE),
        .groups = "drop"
      ) |>
      group_by(Year, MONTH) |>
      mutate(rel_abund = total_cells / sum(total_cells)) |>
      ungroup()

    # Make plot
    p <- ggplot(
      plot_data,
      aes(x = factor(MONTH), y = rel_abund, fill = COMMENTS)
    ) +
      geom_bar(stat = "identity") +
      facet_wrap(~Year, ncol = 1) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = algae_colors, drop = FALSE) + # lock palette
      labs(
        title = paste(
          "Relative abundance of algae types at station",
          station_id
        ),
        x = "Month",
        y = "Relative abundance",
        fill = "Algae type"
      ) +
      theme_minimal()

    # Save plot
    ggsave(
      filename = paste0(output_path, "/", station_id, "_plankton.jpg"),
      plot = p,
      width = 8,
      height = 6,
      dpi = 300
    )

    return(NULL) # keep lapply quiet
  })
}
