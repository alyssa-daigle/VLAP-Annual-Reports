make_chl_tp_secchi <- function(input_path, output_path) {
  # load data
  data <- read_excel(paste0(input_path, "pH_conduc.xlsx"))
  BTC <- read_excel(paste0(input_path, "BTC.xlsx"))

  # get unique trophic statuses
  BTC <- BTC |> distinct(RELLAKE, .keep_all = TRUE)

  # make trophic status threshold df
  trophic_thresholds <- tibble(
    BEST_TROPHIC_CLASS = c("EUTROPHIC", "MESOTROPHIC", "OLIGOTROPHIC"),
    CHLa_thresh = c(11, 5, 3),
    TP_thresh = c(28, 12, 8)
  )

  # combine statuses with thresholds
  data <- data |>
    left_join(BTC, by = c("Rel_Lake" = "RELLAKE")) |>
    left_join(trophic_thresholds, by = "BEST_TROPHIC_CLASS")

  # if output directory doesnt exist, make it
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # make list of unique stationIDs
  station_list <- data |>
    select(StationID) |>
    distinct() |>
    arrange(StationID) |>
    as.list()

  # loop through unique stationIDs
  lapply(station_list, function(station_id) {
    cat(paste0("working on ", station_id, "\n"))

    # selects correct station ID + makes sure theres no NAs
    df_plot <- data |>
      filter(StationID == station_id) |>
      filter(!is.na(CHL_comp) | !is.na(TP_epi) | !is.na(SECCHI)) |>
      arrange(Year)

    # if no data, go to the next stationID
    if (nrow(df_plot) == 0) {
      return()
    }

    # only make a plot if theres some data for 2025 to
    # accounts for lakes currently active in VLAP
    if (!any(df_plot$Year == 2025)) {
      return()
    }

    # define trophic status thresholds for chl and TP
    chl_thresh <- unique(df_plot$CHLa_thresh)
    tp_thresh <- unique(df_plot$TP_thresh)

    # makes sure all years between first active year and current active year are shown
    first_year <- min(df_plot$Year, na.rm = TRUE)
    last_year <- max(df_plot$Year, na.rm = TRUE)
    all_years <- first_year:last_year
    df_plot <- df_plot |>
      complete(
        Year = all_years,
        fill = list(TP_epi = NA, CHL_comp = NA, SECCHI = NA)
      ) |>
      mutate(Year = factor(Year, levels = all_years))

    # scaling for Secchi bars
    max_left <- max(c(df_plot$CHL_comp, df_plot$TP_epi), na.rm = TRUE)
    max_right <- max(df_plot$SECCHI, na.rm = TRUE)
    scale_factor <- ifelse(max_right > 0, max_left / max_right, 1)
    y_max_left <- max_left * 1.5
    y_max_right <- max_right * 1.5
    df_plot <- df_plot |>
      mutate(
        secchi_top = pmin(y_max_left, y_max_right * scale_factor),
        secchi_bottom = pmax(0, (y_max_right - SECCHI) * scale_factor)
      )

    #build plot
    p <- ggplot(df_plot, aes(x = Year)) +

      # Secchi bars mapped to fill
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

      # if theres at least one TP value, add lines between points
      {
        if (sum(!is.na(df_plot$TP_epi)) > 1) {
          geom_line(
            aes(y = TP_epi, group = 1, color = "Phosphorus (µg/L)"),
            size = 0.8
          )
        }
      } +

      # add TP points
      geom_point(
        aes(y = TP_epi, color = "Phosphorus (µg/L)"),
        shape = 17,
        size = 2
      ) +

      # if theres at least one chl value, add lines between points
      {
        if (sum(!is.na(df_plot$CHL_comp)) > 1) {
          geom_line(
            aes(y = CHL_comp, group = 1, color = "Chlorophyll-a (µg/L)"),
            size = 0.8
          )
        }
      } +

      # add chl points
      geom_point(
        aes(y = CHL_comp, color = "Chlorophyll-a (µg/L)"),
        shape = 16,
        size = 2
      ) +

      # add TP threshold lines if threshold exists
      {
        if (!is.na(tp_thresh)) {
          geom_hline(
            yintercept = tp_thresh,
            color = "red4",
            linetype = "dashed",
            size = 0.8,
            show.legend = TRUE
          )
        }
      } +

      # add chl threshold lines if threshold exists
      {
        if (!is.na(chl_thresh)) {
          geom_hline(
            yintercept = chl_thresh,
            color = "springgreen4",
            linetype = "dashed",
            size = 0.8,
            show.legend = TRUE
          )
        }
      } +

      # labeling plot
      labs(
        title = "Historical Chlorophyll-a, Epilimnetic Phosphorus, \n& Transparency Data",
        x = "Year",
        fill = NULL,
        color = NULL
      ) +

      # manually creating transparency icon in legend
      # want it to appear as a blue box with outline under same legend as color
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
          "Chlorophyll-a (µg/L)" = "springgreen4"
          # "TP Threshold" = "red4",
          # "Chl-a Threshold" = "springgreen4"
        )
      ) +
      # scale_linetype_manual(
      #   values = c(
      #     "TP Threshold" = "dashed",
      #     "Chl-a Threshold" = "dashed"
      #   )
      # ) +

      # # Guides for points (no line through legend)
      # guides(
      #   color = guide_legend(override.aes = list(size = 2, linetype = 0)),
      #   fill = guide_legend(
      #     override.aes = list(shape = 22, size = 5, color = "black")
      #   )
      # ) +
      theme(
        legend.position = c(2, 2)
      ) +

      theme_bw() +
      theme_chl_tp_secchi()

    if (sum(!is.na(df_plot$SECCHI)) > 1) {
      p <- p +
        scale_y_continuous(
          # creates chl y axis from predetermined max/min
          name = "Chlorophyll-a & Total Phosphorus (µg/L)",
          limits = c(0, y_max_left),
          expand = c(0, 0),
          breaks = seq(0, ceiling(y_max_left * 2) / 2, by = 5.0),
          labels = function(x) sprintf("%.1f", x),
          # creates secodnary transparency axis scaled off of chl axis
          sec.axis = sec_axis(
            trans = ~ y_max_right - (. / scale_factor),
            name = "Transparency (m)",
            breaks = seq(0, ceiling(y_max_right * 2) / 2, by = 1.0),
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

    # save output
    filename <- paste0(
      station_id,
      "_tp_chl_secchi.png"
    )
    ggsave(
      file.path(output_path, filename),
      plot = p,
      width = 7,
      height = 4,
      dpi = 300
    )
  })
}
