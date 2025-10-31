make_chl_tp_secchi <- function(input_path, output_path) {
  # Create trophic status threshold table
  trophic_thresholds <- tibble(
    BTC = c("EUTROPHIC", "MESOTROPHIC", "OLIGOTROPHIC"),
    CHLa_thresh = c(11, 5, 3),
    TP_thresh = c(28, 12, 8)
  )

  # Combine data with thresholds
  data <- REG |>
    left_join(BTC, by = "lake") |>
    left_join(trophic_thresholds, by = "BTC")

  # Ensure output directory exists
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # List of unique station IDs
  station_list <- data |>
    distinct(stationid) |>
    arrange(stationid) |>
    pull(stationid)

  # Loop through each station ID
  lapply(station_list, function(station_id) {
    message(paste0("Working on chl_tp_secchi for ", station_id, "\n"))

    df_plot <- data |>
      filter(stationid == station_id) |>
      filter(!is.na(CHL_comp) | !is.na(TP_epi) | !is.na(SECCHI)) |>
      arrange(Year)

    if (nrow(df_plot) == 0) {
      return()
    }
    if (!any(df_plot$Year == 2025)) {
      return()
    }

    chl_thresh <- unique(df_plot$CHLa_thresh)
    tp_thresh <- unique(df_plot$TP_thresh)

    first_year <- min(df_plot$Year, na.rm = TRUE)
    last_year <- max(df_plot$Year, na.rm = TRUE)
    all_years <- first_year:last_year

    df_plot <- df_plot |>
      complete(
        Year = all_years,
        fill = list(TP_epi = NA, CHL_comp = NA, SECCHI = NA)
      ) |>
      mutate(Year = factor(Year, levels = all_years))

    max_left <- max(
      c(df_plot$CHL_comp, df_plot$TP_epi, chl_thresh, tp_thresh),
      na.rm = TRUE
    )
    max_right <- max(df_plot$SECCHI, na.rm = TRUE)
    scale_factor <- ifelse(max_right > 0, max_left / max_right, 1)
    y_max_left <- max_left * 1.5
    y_max_right <- max_right * 1.5

    df_plot <- df_plot |>
      mutate(
        secchi_top = pmin(y_max_left, y_max_right * scale_factor),
        secchi_bottom = pmax(0, (y_max_right - SECCHI) * scale_factor)
      )

    # Build plot
    p <- ggplot(df_plot, aes(x = Year)) +
      geom_rect(
        aes(
          xmin = as.numeric(Year) - 0.3,
          xmax = as.numeric(Year) + 0.3,
          ymin = secchi_bottom,
          ymax = secchi_top,
          fill = "Transparency (m)"
        ),
        color = "gray20"
      ) +
      {
        if (sum(!is.na(df_plot$TP_epi)) > 1) {
          geom_line(
            aes(y = TP_epi, group = 1, color = "Phosphorus (µg/L)"),
            size = 0.8
          )
        }
      } +
      geom_point(
        aes(y = TP_epi, color = "Phosphorus (µg/L)"),
        shape = 17,
        size = 2
      ) +
      {
        if (sum(!is.na(df_plot$CHL_comp)) > 1) {
          geom_line(
            aes(y = CHL_comp, group = 1, color = "Chlorophyll-a (µg/L)"),
            size = 0.8
          )
        }
      } +
      geom_point(
        aes(y = CHL_comp, color = "Chlorophyll-a (µg/L)"),
        shape = 16,
        size = 2
      ) +
      {
        if (!is.na(tp_thresh)) {
          geom_hline(
            aes(yintercept = tp_thresh, linetype = "TP Threshold"),
            color = "red4",
            size = 0.8,
            show.legend = TRUE
          )
        }
      } +
      {
        if (!is.na(chl_thresh)) {
          geom_hline(
            aes(yintercept = chl_thresh, linetype = "Chl-a Threshold"),
            color = "springgreen4",
            size = 0.8,
            show.legend = TRUE
          )
        }
      } +
      labs(
        title = "Historical Chlorophyll-a, Epilimnetic Phosphorus, \nand Transparency Data",
        x = "Year",
        fill = NULL,
        color = NULL,
        linetype = NULL
      ) +
      scale_fill_manual(
        values = c("Transparency (m)" = "lightsteelblue2"),
        guide = guide_legend(
          override.aes = list(
            shape = 22,
            size = 2,
            fill = "lightsteelblue2",
            color = "black",
            linetype = 0
          )
        )
      ) +
      scale_color_manual(
        values = c(
          "Phosphorus (µg/L)" = "red4",
          "Chlorophyll-a (µg/L)" = "springgreen4",
          "TP Threshold" = "red4",
          "Chl-a Threshold" = "springgreen4"
        )
      ) +
      scale_linetype_manual(
        values = c("Chl-a Threshold" = "dashed", "TP Threshold" = "dashed")
      ) +
      guides(
        color = guide_legend(override.aes = list(size = 2, linetype = 0))
      ) +
      theme_bw() +
      theme_chl_tp_secchi()

    if (sum(!is.na(df_plot$SECCHI)) > 1) {
      p <- p +
        scale_y_continuous(
          name = "Chlorophyll-a & Total Phosphorus (µg/L)",
          limits = c(0, y_max_left),
          expand = c(0, 0),
          breaks = seq(0, ceiling(y_max_left * 2) / 2, by = 5),
          labels = function(x) sprintf("%.1f", x),
          sec.axis = sec_axis(
            trans = ~ y_max_right - (. / scale_factor),
            name = "Transparency (m)",
            breaks = seq(0, ceiling(y_max_right * 2) / 2, by = 1),
            labels = function(x) sprintf("%.1f", x)
          )
        )
    } else {
      p <- p +
        scale_y_continuous(
          name = "TP & Chl (µg/L)",
          limits = c(0, y_max_left),
          expand = c(0, 0)
        )
    }

    # Save plot and add full PNG border
    filename <- paste0(station_id, "_tp_chl_secchi.png")
    temp_path <- file.path(output_path, filename)

    ggsave(temp_path, plot = p, width = 8, height = 4, dpi = 300, bg = "white")

    img <- magick::image_read(temp_path)
    img_bordered <- magick::image_border(
      img,
      color = "black",
      geometry = "7x7"
    )
    magick::image_write(img_bordered, path = temp_path, format = "png")
  })
}
