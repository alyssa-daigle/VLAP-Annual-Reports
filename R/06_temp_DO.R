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
  stations <- unique(temp_do$Station)

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

      #generate the plot
      p <- ggplot(
        temp_do_long,
        aes(
          x = Value,
          y = Depth,
          color = Variable,
          shape = Month,
          group = interaction(Date, Variable)
        )
      ) +
        geom_path() +
        geom_point(size = 2) +
        scale_y_reverse(breaks = seq(0, ceiling(max_depth), by = 1)) +
        scale_color_manual(
          values = c("Temp" = "darkblue", "DO" = "darkgreen")
        ) +
        labs(
          title = "Dissolved Oxygen & Temperature Profiles 2025",
          x = "Temperature (°C) and Dissolved Oxygen (mg/L)",
          y = "Depth (m)",
          color = "Parameter",
          shape = "Month"
        ) +
        theme_bw(base_size = 14) +
        theme_temp_DO()

      #save output
      filename <- paste0(stn, "_profile.jpg")
      ggsave(
        file.path(output_path, filename),
        plot = p,
        width = 7,
        height = 4,
        dpi = 300
      )
      return(NULL)
    }
  )
}
