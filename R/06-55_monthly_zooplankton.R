# =========================================================
# MONTHLY ZOOPLANKTON RELATIVE ABUNDANCE
#
# Uses 2026 data where Type = "Z"
# The column named Percent contains RAW COUNTS, not percentages.
#
# - One month of data: pie chart with percentage labels
# - Two or more months: stacked monthly bar chart
# - Relative abundance calculated from raw counts
# - Pie labels sum to exactly 100% after rounding
# - Separate plot for each lake and station
# - Filename: WQDStationID_zooplankton.png
# =========================================================

make_zooplankton_monthly <- function(INPUT_PATH, OUTPUT_PATH) {
  # -------------------------------------------------------
  # Output directory
  # -------------------------------------------------------
  monthly_output <- file.path(
    OUTPUT_PATH,
    "zooplankton_monthly"
  )

  if (!dir.exists(monthly_output)) {
    dir.create(monthly_output, recursive = TRUE)
  }

  library(ggpattern)

  # -------------------------------------------------------
  # Read data
  # -------------------------------------------------------
  plankton_file <- file.path(
    INPUT_PATH,
    paste0("Historical_Phytoplankton_Data_Thru", YEAR, ".xlsx")
  )

  data <- readxl::read_excel(plankton_file)

  # -------------------------------------------------------
  # Identify station ID column
  # -------------------------------------------------------
  station_column <- "WQDStationID"

  if (!station_column %in% names(data)) {
    stop(
      "Station ID column 'WQDStationID' was not found in the input file."
    )
  }

  # Confirm required columns are present
  required_columns <- c(
    "Type",
    "date-time",
    "lake",
    "DIVISION",
    "Percent"
  )

  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # -------------------------------------------------------
  # Filter to 2026 zooplankton records
  # -------------------------------------------------------
  data <- data |>
    dplyr::filter(
      toupper(trimws(as.character(Type))) == "Z",
      lubridate::year(`date-time`) == 2026
    ) |>
    dplyr::mutate(
      year = lubridate::year(`date-time`),
      month = lubridate::month(`date-time`),
      station_id = trimws(as.character(.data[[station_column]])),

      # Percent contains raw organism counts, not percentages
      count = suppressWarnings(as.numeric(Percent))
    )

  message(
    "Found ",
    nrow(data),
    " zooplankton records from 2026."
  )

  if (nrow(data) == 0) {
    message("No 2026 zooplankton records found. Skipping plots.")
    return(invisible(NULL))
  }

  # -------------------------------------------------------
  # Standardize zooplankton groups and validate counts
  # -------------------------------------------------------
  data <- data |>
    dplyr::mutate(
      group = toupper(trimws(as.character(DIVISION))),

      group = dplyr::case_when(
        group == "CRUSTACEAN" ~ "CRUSTACEAN",
        group == "ROTIFER" ~ "ROTIFER",
        group == "COPEPOD" ~ "COPEPOD",
        group == "CILIATE" ~ "CILIATE",
        group %in% c("CLADOERAN", "CLADOCERAN") ~ "CLADOCERAN",
        TRUE ~ NA_character_
      ),

      lake_name = as.character(lake),

      station_id = dplyr::if_else(
        is.na(station_id) | station_id == "",
        "UNKNOWN_STATION",
        station_id
      )
    ) |>
    dplyr::filter(
      !is.na(group),
      !is.na(count),
      is.finite(count),
      count >= 0
    )

  if (nrow(data) == 0) {
    message(
      "No records with valid nonnegative counts matched the five groups."
    )
    return(invisible(NULL))
  }

  # -------------------------------------------------------
  # Calculate relative abundance from RAW COUNTS
  #
  # Step 1: Sum raw counts within each group.
  # Step 2: Sum counts across all included groups for each
  #         lake, station, year, and month.
  # Step 3: Divide each group's count by that total.
  #
  # Relative abundances for each lake/station/month sum to 1.
  # -------------------------------------------------------
  rel_abund <- data |>
    dplyr::group_by(
      lake_name,
      station_id,
      year,
      month,
      group
    ) |>
    dplyr::summarise(
      group_count = sum(count),
      .groups = "drop"
    ) |>
    dplyr::group_by(
      lake_name,
      station_id,
      year,
      month
    ) |>
    dplyr::mutate(
      total_count = sum(group_count),

      rel_abundance = dplyr::if_else(
        total_count > 0,
        group_count / total_count,
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(rel_abundance)) |>
    dplyr::select(
      RELLAKE = lake_name,
      station_id,
      year,
      month,
      group,
      group_count,
      total_count,
      rel_abundance
    )

  data <- rel_abund

  if (nrow(data) == 0) {
    message("No valid relative-abundance data available.")
    return(invisible(NULL))
  }

  # -------------------------------------------------------
  # Colors, labels, patterns, and legend order
  # -------------------------------------------------------
  zoo_colors <- c(
    "CLADOCERAN" = "#F0E442",
    "CILIATE" = "#56B4E9",
    "COPEPOD" = "#9400D3",
    "CRUSTACEAN" = "#D55E00",
    "ROTIFER" = "#009E73"
  )

  zoo_labels <- c(
    "CRUSTACEAN" = "Crustaceans",
    "ROTIFER" = "Rotifers",
    "COPEPOD" = "Copepods",
    "CILIATE" = "Ciliates",
    "CLADOCERAN" = "Cladocerans"
  )

  zoo_patterns <- c(
    "CRUSTACEAN" = "none",
    "ROTIFER" = "stripe",
    "COPEPOD" = "stripe",
    "CILIATE" = "crosshatch",
    "CLADOCERAN" = "circle"
  )

  zoo_pattern_angles <- c(
    "CRUSTACEAN" = 30,
    "ROTIFER" = 45,
    "COPEPOD" = 135,
    "CILIATE" = 0,
    "CLADOCERAN" = 45
  )

  legend_order <- names(zoo_colors)

  month_labels <- c(
    "1" = "January",
    "2" = "February",
    "3" = "March",
    "4" = "April",
    "5" = "May",
    "6" = "June",
    "7" = "July",
    "8" = "August",
    "9" = "September",
    "10" = "October",
    "11" = "November",
    "12" = "December"
  )

  # -------------------------------------------------------
  # Identify unique lake and station combinations
  # -------------------------------------------------------
  plot_keys <- data |>
    dplyr::distinct(RELLAKE, station_id) |>
    dplyr::arrange(RELLAKE, station_id)

  message(
    "Generating plots for ",
    nrow(plot_keys),
    " lake and station combinations."
  )

  # -------------------------------------------------------
  # Generate one plot per lake and station
  # -------------------------------------------------------
  lapply(seq_len(nrow(plot_keys)), function(i) {
    lake_name <- plot_keys$RELLAKE[i]
    current_station_id <- plot_keys$station_id[i]

    message(
      "Working on ",
      lake_name,
      " | Station: ",
      current_station_id
    )

    plot_data <- data |>
      dplyr::filter(
        RELLAKE == lake_name,
        station_id == current_station_id
      )

    months_with_data <- sort(unique(plot_data$month))

    single_month <- length(months_with_data) == 1

    month_names <- unname(
      month_labels[as.character(months_with_data)]
    )

    # -----------------------------------------------------
    # Prepare plotting data
    # -----------------------------------------------------
    plot_data <- plot_data |>
      dplyr::mutate(
        group = factor(group, levels = legend_order),

        month = factor(
          month,
          levels = months_with_data,
          labels = month_names
        )
      ) |>
      tidyr::complete(
        month = factor(month_names, levels = month_names),
        group = factor(legend_order, levels = legend_order),
        fill = list(
          rel_abundance = 0,
          group_count = 0,
          total_count = 0
        )
      ) |>
      dplyr::mutate(
        group = factor(group, levels = legend_order),
        month = factor(month, levels = month_names)
      )

    # -----------------------------------------------------
    # Shared legend settings
    # -----------------------------------------------------
    zoo_legend <- ggplot2::guides(
      fill = ggplot2::guide_legend(
        title = "Group",
        ncol = 1,
        byrow = TRUE,
        override.aes = list(
          pattern = unname(zoo_patterns[legend_order]),
          pattern_angle = unname(zoo_pattern_angles[legend_order]),
          pattern_fill = "grey25",
          pattern_colour = NA,
          pattern_density = 0.12,
          pattern_spacing = 0.01,
          pattern_key_scale_factor = 1.8
        )
      ),
      pattern = "none",
      pattern_angle = "none"
    )

    # -----------------------------------------------------
    # SINGLE MONTH: PIE CHART
    # -----------------------------------------------------
    if (single_month) {
      message(
        "  -> One month of data. Creating labeled pie chart."
      )

      # Keep only groups with positive counts
      pie_data <- plot_data |>
        dplyr::filter(
          rel_abundance > 0,
          group_count > 0
        ) |>
        dplyr::mutate(
          group = factor(group, levels = legend_order)
        )

      # Calculate whole-number labels that sum to exactly 100%.
      # Largest-remainder rounding distributes leftover percentage
      # points to groups with the largest fractional remainders.
      pie_data <- pie_data |>
        dplyr::mutate(
          exact_percent = rel_abundance * 100,
          base_percent = floor(exact_percent),
          fractional_remainder = exact_percent - base_percent
        )

      remaining_points <- round(
        100 - sum(pie_data$base_percent)
      )

      pie_data <- pie_data |>
        dplyr::arrange(
          dplyr::desc(fractional_remainder),
          group
        ) |>
        dplyr::mutate(
          displayed_percent = base_percent +
            as.integer(dplyr::row_number() <= remaining_points),

          percentage_label = paste0(
            displayed_percent,
            "%"
          ),

          # Preserve the original factor order for the pie
          group = factor(group, levels = legend_order)
        )

      # Ensure the displayed labels total exactly 100%
      message(
        "  -> Pie labels total ",
        sum(pie_data$displayed_percent),
        "%."
      )

      p_main <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = "",
          y = rel_abundance,
          fill = group,
          pattern = group,
          pattern_angle = group
        )
      ) +
        ggpattern::geom_bar_pattern(
          stat = "identity",
          position = "stack",
          width = 1,
          color = "white",
          linewidth = 0.3,
          pattern_fill = "grey25",
          pattern_colour = NA,
          pattern_density = 0.12,
          pattern_spacing = 0.04,
          pattern_key_scale_factor = 1.8
        ) +

        # Place each label at the midpoint of its actual slice.
        # Use the same y values and group mapping as the pie itself.
        ggplot2::geom_label(
          data = pie_data,
          mapping = ggplot2::aes(
            x = "",
            y = rel_abundance,
            group = group,
            label = percentage_label
          ),
          inherit.aes = FALSE,
          position = ggplot2::position_stack(vjust = 0.5),
          color = "black",
          fill = "white",
          label.size = 0.2,
          size = 3.5,
          fontface = "bold",
          show.legend = FALSE
        ) +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::scale_fill_manual(
          values = zoo_colors,
          labels = zoo_labels,
          drop = FALSE
        ) +
        ggpattern::scale_pattern_manual(
          values = zoo_patterns,
          drop = FALSE
        ) +
        ggpattern::scale_pattern_angle_manual(
          values = zoo_pattern_angles,
          drop = FALSE
        ) +
        zoo_legend +
        ggplot2::labs(
          title = paste0(
            lake_name,
            " Zooplankton Population - ",
            month_names,
            " 2026"
          ),
          subtitle = paste0(
            "Station: ",
            current_station_id
          ),
          fill = NULL
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = "right",
          plot.title = ggplot2::element_text(
            family = "Calibri",
            face = "bold",
            size = 14,
            hjust = 0.5
          ),
          plot.subtitle = ggplot2::element_text(
            family = "Calibri",
            size = 10,
            hjust = 0.5
          ),
          legend.title = ggplot2::element_text(
            family = "Calibri",
            face = "bold"
          )
        )

      # -----------------------------------------------------
      # TWO OR MORE MONTHS: STACKED BAR CHART
      # -----------------------------------------------------
    } else {
      message(
        "  -> Found ",
        length(months_with_data),
        " months. Creating stacked bar chart."
      )

      p_main <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = month,
          y = rel_abundance,
          fill = group,
          pattern = group,
          pattern_angle = group
        )
      ) +
        ggpattern::geom_bar_pattern(
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
        ggplot2::scale_y_continuous(
          labels = scales::percent,
          breaks = seq(0, 1, by = 0.1),
          expand = c(0, 0),
          limits = c(0, 1)
        ) +
        ggplot2::scale_fill_manual(
          values = zoo_colors,
          labels = zoo_labels,
          drop = FALSE
        ) +
        ggpattern::scale_pattern_manual(
          values = zoo_patterns,
          drop = FALSE
        ) +
        ggpattern::scale_pattern_angle_manual(
          values = zoo_pattern_angles,
          drop = FALSE
        ) +
        zoo_legend +
        ggplot2::labs(
          title = paste0(
            lake_name,
            " Monthly Zooplankton Population - 2026"
          ),
          subtitle = paste0(
            "Station: ",
            current_station_id
          ),
          x = "Month",
          y = "Relative Abundance",
          fill = NULL
        ) +
        ggplot2::theme_bw() +
        theme_plankton() +
        ggplot2::theme(
          plot.subtitle = ggplot2::element_text(
            family = "Calibri",
            size = 10
          ),
          legend.title = ggplot2::element_text(
            family = "Calibri",
            face = "bold"
          )
        )
    }

    # -----------------------------------------------------
    # Filename: station ID only
    # -----------------------------------------------------
    filename <- paste0(
      current_station_id,
      "_zooplankton.png"
    )

    filename <- gsub(
      "[^A-Za-z0-9_.-]",
      "_",
      filename
    )

    # -----------------------------------------------------
    # Save plot
    # -----------------------------------------------------
    temp_path <- file.path(
      monthly_output,
      filename
    )

    ggplot2::ggsave(
      filename = temp_path,
      plot = p_main,
      width = 8,
      height = 4,
      dpi = 300,
      bg = "white"
    )

    # -----------------------------------------------------
    # Add black border
    # -----------------------------------------------------
    img <- magick::image_read(temp_path)

    img_bordered <- magick::image_border(
      img,
      color = "black",
      geometry = "7x7"
    )

    magick::image_write(
      img_bordered,
      path = temp_path,
      format = "png"
    )

    message("  -> Saved: ", filename)
  })

  message(
    "Finished generating zooplankton plots in: ",
    monthly_output
  )

  invisible(NULL)
}
