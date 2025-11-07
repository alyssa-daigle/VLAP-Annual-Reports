make_temp_DO <- function(input_path, output_path) {
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(magick)
  library(scales)

  # Load data
  data <- read_excel(paste0(input_path, "master-DO-2025.xlsm"))

  # Create output directory if missing
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Clean and prepare
  temp_do <- data |>
    select(Depth, Date, Station, DO, Temp...12) |>
    rename(Temp = `Temp...12`) |>
    mutate(
      Date = as.Date(Date),
      Month = factor(format(Date, "%B"), levels = month.name, ordered = TRUE)
    ) |>
    filter(!is.na(Station))

  stations <- sort(unique(temp_do$Station))

  lapply(stations, function(stn) {
    message(paste0("Working on temp_DO for ", stn, "\n"))

    temp_do_stn <- temp_do |> filter(Station == stn)

    # Average duplicate profiles
    temp_do_avg <- temp_do_stn |>
      group_by(Date, Depth) |>
      summarise(
        Temp = mean(Temp, na.rm = TRUE),
        DO = mean(DO, na.rm = TRUE),
        Month = first(Month),
        .groups = "drop"
      )

    max_depth <- max(temp_do_avg$Depth, na.rm = TRUE)
    months_present <- sort(unique(temp_do_avg$Month))
    n_months <- length(months_present)

    # Shapes (for months)
    shape_values <- c(21, 22, 23, 24, 25)[seq_len(n_months)]

    # --- Automatic x-axis limits ---
    temp_range <- range(temp_do_avg$Temp, na.rm = TRUE)
    temp_limits <- if (diff(temp_range) < 1) {
      # nearly isothermal
      c(floor(temp_range[1] - 1), ceiling(temp_range[2] + 1))
    } else {
      temp_range
    }

    DO_range <- range(temp_do_avg$DO, na.rm = TRUE)
    DO_limits <- if (diff(DO_range) < 1) {
      # nearly uniform DO
      c(floor(DO_range[1] - 1), ceiling(DO_range[2] + 1))
    } else {
      DO_range
    }

    # Temperature plot
    p_temp <- ggplot(
      temp_do_avg,
      aes(x = Temp, y = Depth, group = Date, shape = Month, color = Month)
    ) +
      geom_path(aes(color = Month)) +
      geom_point(aes(fill = Month), size = 1.5, stroke = 0.5) +
      scale_color_manual(
        values = scales::seq_gradient_pal("lightgreen", "darkgreen")(seq(
          0,
          1,
          length.out = n_months
        ))
      ) +
      scale_fill_manual(
        values = scales::seq_gradient_pal("lightgreen", "darkgreen")(seq(
          0,
          1,
          length.out = n_months
        ))
      ) +
      scale_shape_manual(
        values = shape_values,
        guide = guide_legend(
          override.aes = list(color = "black", fill = "black")
        )
      ) +
      scale_y_reverse(breaks = seq(0, ceiling(max_depth), by = 1)) +
      scale_x_continuous(limits = temp_limits) +
      labs(x = "Temperature (°C)", y = "Depth (m)") +
      theme_bw(base_size = 13) +
      theme_temp_DO() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank()
      )

    # DO plot
    p_DO <- ggplot(
      temp_do_avg,
      aes(x = DO, y = Depth, group = Date, shape = Month, color = Month)
    ) +
      geom_path(aes(color = Month)) +
      geom_point(aes(fill = Month), size = 1.5, stroke = 0.5) +
      scale_color_manual(
        values = scales::seq_gradient_pal("lightblue", "darkblue")(seq(
          0,
          1,
          length.out = n_months
        ))
      ) +
      scale_fill_manual(
        values = scales::seq_gradient_pal("lightblue", "darkblue")(seq(
          0,
          1,
          length.out = n_months
        ))
      ) +
      scale_shape_manual(
        values = shape_values,
        guide = guide_legend(
          override.aes = list(color = "black", fill = "black")
        )
      ) +
      scale_y_reverse(breaks = seq(0, ceiling(max_depth), by = 1)) +
      scale_x_continuous(limits = DO_limits) +
      labs(x = "Dissolved Oxygen (mg/L)", y = "") +
      theme_bw(base_size = 13) +
      theme_temp_DO() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank()
      )

    # Extract shared legend (black shapes only)
    legend <- cowplot::get_legend(p_temp)

    # Remove legends from individual plots
    p_temp_noleg <- p_temp + theme(legend.position = "none")
    p_DO_noleg <- p_DO + theme(legend.position = "none")

    # Combine plots side-by-side
    combined_panels <- plot_grid(
      p_temp_noleg,
      p_DO_noleg,
      ncol = 2,
      align = "h",
      rel_widths = c(1, 1)
    )

    # Add shared title + legend
    combined_final <- plot_grid(
      ggdraw() +
        draw_label(
          "Temperature and Dissolved Oxygen Profiles (2025)",
          fontface = "bold",
          size = 13,
          x = 0.5,
          hjust = 0.5
        ),
      combined_panels,
      legend,
      ncol = 1,
      rel_heights = c(0.12, 1, 0.18)
    )

    # Save
    filename <- paste0(stn, "_profile.png")
    temp_path <- file.path(output_path, filename)

    ggsave(
      temp_path,
      plot = combined_final,
      width = 8,
      height = 4,
      dpi = 300,
      bg = "white"
    )

    # Add border
    img <- magick::image_read(temp_path)
    img_bordered <- magick::image_border(img, color = "black", geometry = "7x7")
    magick::image_write(img_bordered, path = temp_path, format = "png")
  })
}
