make_temp_DO <- function(input_path, output_path) {
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

  # List of stations
  stations <- sort(unique(temp_do$Station))

  # Loop through each station
  lapply(stations, function(stn) {
    message(paste0("Working on temp_DO for ", stn, "\n"))

    temp_do_stn <- temp_do |> filter(Station == stn)

    # Average duplicate profiles if they exist
    temp_do_avg <- temp_do_stn |>
      group_by(Date, Depth) |>
      summarise(
        Temp = mean(Temp, na.rm = TRUE),
        DO = mean(DO, na.rm = TRUE),
        Month = first(Month),
        .groups = "drop"
      )

    # Reshape to long format
    temp_do_long <- temp_do_avg |>
      pivot_longer(
        cols = c(Temp, DO),
        names_to = "Variable",
        values_to = "Value"
      )

    max_depth <- max(temp_do_long$Depth, na.rm = TRUE)

    # Capture months present for gradient and shapes
    months_present <- sort(unique(temp_do_long$Month))
    n_months <- length(months_present)
    shape_values <- 1:n_months

    # Create color palettes per parameter
    month_colors_temp <- scales::seq_gradient_pal(
      "lightgreen",
      "darkgreen"
    )(seq(0, 1, length.out = n_months))
    month_colors_DO <- scales::seq_gradient_pal("lightblue", "darkblue")(seq(
      0,
      1,
      length.out = n_months
    ))
    names(month_colors_temp) <- months_present
    names(month_colors_DO) <- months_present

    # Generate plot
    p <- ggplot(
      temp_do_long,
      aes(x = Value, y = Depth, group = interaction(Date, Variable))
    ) +

      # Temperature
      geom_path(
        data = filter(temp_do_long, Variable == "Temp"),
        aes(color = Month, shape = Month)
      ) +
      geom_point(
        data = filter(temp_do_long, Variable == "Temp"),
        aes(color = Month, shape = Month),
        size = 2
      ) +
      scale_color_manual(values = month_colors_temp, name = "Temp") +
      scale_shape_manual(values = shape_values, name = "Temp") +

      ggnewscale::new_scale_color() +

      # DO
      geom_path(
        data = filter(temp_do_long, Variable == "DO"),
        aes(color = Month, shape = Month)
      ) +
      geom_point(
        data = filter(temp_do_long, Variable == "DO"),
        aes(color = Month, shape = Month),
        size = 2
      ) +
      scale_color_manual(values = month_colors_DO, name = "DO") +
      scale_shape_manual(values = shape_values, name = "DO") +

      # Axes and labels
      scale_y_reverse(breaks = seq(0, ceiling(max_depth), by = 1)) +
      scale_x_continuous(
        breaks = seq(
          floor(min(temp_do_long$Value, na.rm = TRUE)),
          ceiling(max(temp_do_long$Value, na.rm = TRUE)),
          by = 2
        )
      ) +
      labs(
        title = "Dissolved Oxygen & Temperature Profiles 2025",
        x = "Temperature (°C) and Dissolved Oxygen (mg/L)",
        y = "Depth (m)"
      ) +
      theme_bw(base_size = 14) +
      theme_temp_DO()

    # Save plot with border
    filename <- paste0(stn, "_profile.png")
    temp_path <- file.path(output_path, filename)

    ggsave(temp_path, plot = p, width = 8, height = 4, dpi = 300, bg = "white")

    img <- magick::image_read(temp_path)
    img_bordered <- magick::image_border(img, color = "black", geometry = "7x7")
    magick::image_write(img_bordered, path = temp_path, format = "png")
  })
}
