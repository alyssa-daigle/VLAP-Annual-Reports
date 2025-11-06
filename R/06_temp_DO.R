make_temp_DO <- function(input_path, output_path) {
  #load data
  data <- read_excel(paste0(input_path, "master-DO-2025.xlsm"))

  #if output directory doesnt exist, make it
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  #clean and prepare
  temp_do <- data |>
    select(Depth, Date, Station, DO, Temp...12) |>
    rename(Temp = `Temp...12`) |>
    mutate(
      Date = as.Date(Date),
      Month = format(Date, "%B"),
      Month = factor(Month, levels = month.name, ordered = TRUE)
    ) |>
    filter(!is.na(Station))

  #get list of unique station names
  stations <- sort(unique(temp_do$Station))

  #loop through each station and make a temp/DO plot
  lapply(
    stations,
    function(stn) {
      message(paste0("working on temp_DO for ", stn, "\n"))
      temp_do_stn <- temp_do |>
        filter(Station == stn) |>
        select(Depth, Date, Month, Station, DO, Temp)

      #average duplicate profiles, if they exist
      temp_do_avg <- temp_do_stn |>
        group_by(Date, Depth) |>
        summarise(
          Temp = mean(Temp, na.rm = TRUE),
          DO = mean(DO, na.rm = TRUE),
          Month = first(Month),
          .groups = "drop"
        )

      # reshape to long format
      temp_do_long <- temp_do_avg |>
        pivot_longer(
          cols = c(Temp, DO),
          names_to = "Variable",
          values_to = "Value"
        )

      #compute max depth for this station
      max_depth <- max(temp_do_long$Depth, na.rm = TRUE)

      library(ggnewscale)

      p <- ggplot(
        temp_do_long,
        aes(
          x = Value,
          y = Depth,
          shape = Month,
          group = interaction(Date, Variable)
        )
      ) +

        # Temperature
        geom_path(
          data = filter(temp_do_long, Variable == "Temp"),
          aes(color = Date),
          show.legend = TRUE # hide color from legend
        ) +
        geom_point(
          data = filter(temp_do_long, Variable == "Temp"),
          aes(color = Date),
          size = 2,
          show.legend = TRUE
        ) +
        scale_color_gradient(low = "lightblue", high = "darkblue") + # gradient for Temp

        ggnewscale::new_scale_color() + # new scale for DO

        # DO
        geom_path(
          data = filter(temp_do_long, Variable == "DO"),
          aes(color = Date),
          show.legend = TRUE
        ) +
        geom_point(
          data = filter(temp_do_long, Variable == "DO"),
          aes(color = Date),
          size = 2,
          show.legend = TRUE
        ) +
        scale_color_gradient(low = "lightgreen", high = "darkgreen") + # gradient for DO

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
          y = "Depth (m)",
          shape = "Month" # keep shapes in legend
        ) +
        guides(shape = guide_legend(order = 1)) +
        theme_bw(base_size = 14) +
        theme_temp_DO()

      # Save plot and add full PNG border
      filename <- paste0(stn, "_profile.png")
      temp_path <- file.path(output_path, filename)

      ggsave(
        temp_path,
        plot = p,
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
    }
  )
}
