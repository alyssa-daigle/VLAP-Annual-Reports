make_pH_conduc <- function(input_path, output_path) {
  #if output directory doesnt exist, make it
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  data <- REG_plot

  #get unique stationIDs
  station_list <- data |>
    distinct(stationid) |>
    arrange(stationid) |>
    pull(stationid)

  #loop through each stationid
  lapply(
    station_list,
    function(station_id) {
      temp_path <- file.path(
        output_path,
        paste0(station_id, "_pH_conduc.png")
      )
      message(paste0("working on pH_cond for ", station_id, "\n"))

      #select correct station ID + remove NAs
      df_plot <- data |>
        filter(stationid == station_id) |>
        filter(!is.na(PH_epi) | !is.na(SPCD_epi)) |>
        arrange(Year)

      #skip if no data
      if (nrow(df_plot) == 0) {
        return(NULL)
      }

      # oly plot if there's 2025 data
      if (!any(df_plot$Year == 2025)) {
        return(NULL)
      }

      #plot all years between first and last active year
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

      #skip station if invalid numeric values
      if (!is.finite(min_pH) | !is.finite(max_pH) | !is.finite(max_cond)) {
        return(NULL)
      }

      #compute scaling for pH and conductivity
      min_pH <- floor(min_pH * 10) / 10 - 0.2
      max_pH <- ceiling(max_pH * 10) / 10 + 0.2
      scale_factor <- max_cond / (max_pH - min_pH)
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
        theme(legend.position = "none") +
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

      # --- Save ggplot (no legend) ---
      temp_file <- tempfile(fileext = ".png")
      ggsave(
        temp_file,
        plot = p,
        width = 8,
        height = 4,
        dpi = 300,
        bg = "white"
      )

      # --- Reopen and add base R legend ---
      png(
        temp_path,
        width = 8,
        height = 4,
        units = "in",
        res = 300,
        bg = "white"
      )
      par(mar = c(0, 0, 0, 0))
      plot.new()
      par(usr = c(0, 1, 0, 1))

      # Draw the ggplot image as the background
      img_raster <- as.raster(
        magick::image_read(temp_file) |> magick::image_convert("png")
      )

      rasterImage(img_raster, 0, 0, 1, 1)

      # Add custom legend (Base R)
      legend(
        x = "topright", # position of the legend
        inset = c(0.04, 0.03), #  first = horizontal (left/right), second = vertical (up/down)
        legend = c(
          "pH",
          "Conductivity"
        ),
        pch = c(0, 21), # symbol/point type
        col = c("black", "red3"), # color of symbols and/or lines
        pt.bg = c(NA, "red3"), # background/fill color for symbols
        lty = c(0, 1), # line type
        lwd = c(1, 1.1), # line width
        pt.cex = c(1.25, 0.8), # size of the point symbols
        bty = "o", # legend box type
        y.intersp = 1.2, # vertical spacing between legend entries
        cex = 0.7, # text size
        text.font = 2
      )
      dev.off()

      # --- Add black border ---
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
