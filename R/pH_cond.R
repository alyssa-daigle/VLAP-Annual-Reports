make_pH_conduc <- function(input_path, output_path) {
  #load data
  data <- read_excel(paste0(input_path, "pH_conduc.xlsx"))

  #if output directory doesnt exist, make it
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  #get unique stationIDs
  station_list <- data |>
    select(StationID) |>
    distinct() |>
    arrange(StationID)

  #loop through each stationID
  for (i in 1:nrow(station_list)) {
    station_id <- station_list$StationID[i]

    #only get data for one stationID at a time
    df_plot <- data |>
      filter(StationID == station_id) |>
      filter(!is.na(PH_epi) | !is.na(SPCD_epi)) |>
      arrange(Year)

    #move on if no data
    if (nrow(df_plot) == 0) {
      next
    }
    #only plot if there's 2025 data
    #this allows us to report only on active VLAP lakes not not unnecessarily generate plots
    if (!any(df_plot$Year == 2025)) {
      next
    }

    #plot all years between first active and most recent active year
    first_year <- min(df_plot$Year, na.rm = TRUE)
    last_year <- max(df_plot$Year, na.rm = TRUE)
    all_years <- first_year:last_year
    df_plot <- df_plot |>
      complete(Year = all_years, fill = list(PH_epi = NA, SPCD_epi = NA)) |>
      mutate(Year = factor(Year, levels = all_years))

    #determine pH and conductivity ranges
    min_pH <- min(df_plot$PH_epi, na.rm = TRUE)
    max_pH <- max(df_plot$PH_epi, na.rm = TRUE)
    max_cond <- max(df_plot$SPCD_epi, na.rm = TRUE)

    #checking for invalid numeric values and skipping the station if any are found
    if (!is.finite(min_pH) | !is.finite(max_pH) | !is.finite(max_cond)) {
      next
    }

    #compute the proper scaling for pH and conductivity axes
    min_pH <- floor(min_pH * 10) / 10 - 0.2
    max_pH <- ceiling(max_pH * 10) / 10 + 0.2
    scale_factor <- (max_cond) / (max_pH - min_pH)
    y_max_left <- max_cond * 1.1
    pH_range <- c(min_pH, max_pH)

    #start building the plot
    p <- ggplot(df_plot, aes(x = Year)) +

      geom_col(
        aes(y = (PH_epi - min_pH) * scale_factor, fill = "pH"),
        color = "black",
        width = 0.6
      ) +

      {
        if (sum(!is.na(df_plot$SPCD_epi)) > 1) {
          geom_line(aes(y = SPCD_epi, group = 1, color = "cond"), size = 0.5)
        }
      } +

      geom_point(aes(y = SPCD_epi, color = "cond"), size = 1.5) +

      labs(
        title = "Historical Trend Epilimnetic Conductivity and pH",
        x = "Year",
        fill = NULL,
        color = NULL
      ) +

      theme_bw() +
      theme_pH_conduc() +
      theme(
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.25, "cm")
      )

    #add second y axis using the scale factor
    p <- p +
      scale_y_continuous(
        name = "Conductivity (µS/cm)",
        limits = c(0, y_max_left),
        expand = c(0, 0),
        sec.axis = sec_axis(
          ~ . / scale_factor + min_pH,
          name = "pH",
          breaks = pretty(pH_range)
        )
      )

    #save
    filename <- paste0(
      station_id,
      "_pH_conduc.png"
    )
    ggsave(
      file.path(output_path, filename),
      plot = p,
      width = 7,
      height = 4,
      dpi = 300
    )
  }
}
