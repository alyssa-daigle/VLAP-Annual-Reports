make_plankton <- function(input_path, output_path) {
  # load data
  data <- read_excel(paste0(input_path, "VLAPPlanktonGraphicYear-2024.xlsx"))

  # if output directory doesn't exist, make it
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # get list of stations
  stations <- unique(data$WQDStationID)

  # loop over stations
  lapply(stations, function(station_id) {
    # prepare plot_data
    plot_data <- data |>
      filter(WQDStationID == station_id) |>
      group_by(Year, MONTH, COMMENTS) |> # COMMENTS actually contains algal taxa
      summarise(
        total_cells = sum(`Cell Count`, na.rm = TRUE),
        .groups = "drop_last" # keep Year, MONTH grouping for next step
      ) |>
      mutate(
        rel_abund = total_cells / sum(total_cells), # relative abundance per month
        COMMENTS = factor(COMMENTS, levels = names(algae_colors)), # keeps ordering of algae for legend
        COMMENTS_ordered = fct_reorder(COMMENTS, rel_abund, .desc = FALSE) # sort most to least abundant
      ) |>
      ungroup() |>
      complete(
        # ensures all legend items are always present even if not in plot
        Year,
        MONTH,
        COMMENTS = names(algae_colors),
        fill = list(total_cells = 0, rel_abund = 0)
      )

    # main plot, without legend
    p_main <- ggplot(
      plot_data,
      aes(x = factor(MONTH), y = rel_abund, fill = COMMENTS_ordered)
    ) +
      geom_bar(stat = "identity") +
      scale_y_continuous(
        labels = scales::percent,
        breaks = seq(0, 1, by = 0.1), # break every 10% until 100%
        expand = c(0, 0), # allows bars to start at zero
        limits = c(0, 1) # 0 to 100%
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

    # legend only plot
    p_legend <- ggplot(plot_data, aes(x = 1, y = 1, fill = COMMENTS)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(
        values = algae_colors,
        labels = algae_labels,
        drop = FALSE
      ) +
      labs(fill = NULL) + # removes title from legend
      theme_void() +
      theme_plankton_legend() +
      guides(fill = guide_legend(ncol = 1))

    # extract legend
    legend <- get_legend(p_legend)

    # combine main plot + fixed legend
    final_plot <- plot_grid(p_main, legend, rel_widths = c(5, 1))

    # save plot
    ggsave(
      filename = paste0(output_path, "/", station_id, "_plankton.jpg"),
      plot = p,
      width = 8,
      height = 6,
      dpi = 300
    )

    return(NULL)
  })
}
