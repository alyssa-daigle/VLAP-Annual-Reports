make_chloride <- function(input_path, output_path) {
  # -----------------------------
  # Okabe–Ito color-blind–safe palette
  # -----------------------------
  okabe_ito <- c(
    "#000000", # black
    "#E69F00", # orange
    "#56B4E9", # sky blue
    "#009E73", # bluish green
    "#F0E442", # yellow
    "#0072B2", # blue
    "#D55E00", # vermillion
    "#CC79A7" # reddish purple
  )

  # --- create chloride subdirectory ---
  chloride_path <- file.path(output_path)
  if (!dir.exists(chloride_path)) {
    dir.create(chloride_path, recursive = TRUE)
  }

  # --- subset to chloride only ---
  cya_cl <- CYA_long |>
    filter(param_depth == "Chloride (mg/L)")

  if (nrow(cya_cl) == 0) {
    stop("No chloride data found in CYA_long.")
  }

  # --- unique lake × town combinations ---
  lake_town_tbl <- cya_cl |>
    distinct(RELLAKE, TOWN) |>
    arrange(RELLAKE, TOWN)

  lapply(
    seq_len(nrow(lake_town_tbl)),
    function(i) {
      lake <- lake_town_tbl$RELLAKE[i]
      town <- lake_town_tbl$TOWN[i]

      message("Working on chloride for ", lake, " (", town, ")\n")

      # --- average chloride per station per year ---
      df_plot <- cya_cl |>
        filter(RELLAKE == lake, TOWN == town) |>
        group_by(STATIONID, STATNAME, Year) |>
        summarise(
          avg_chloride = mean(avg_result, na.rm = TRUE),
          .groups = "drop"
        ) |>
        filter(
          STATNAME == "Epilimnion" |
            str_detect(tolower(STATNAME), "inlet")
        ) |>
        arrange(Year)

      # --- skip if no usable data or no 2025 data ---
      if (nrow(df_plot) == 0 || !2025 %in% df_plot$Year) {
        message("Skipping ", lake, " (", town, ") - no 2025 data.\n")
        return(NULL)
      }

      # --- ensure Year is numeric ---
      df_plot <- df_plot |>
        mutate(Year = as.numeric(Year))

      # --- fill in missing years per series ---
      df_plot <- df_plot |>
        group_by(series = STATNAME) |>
        complete(
          Year = seq(min(Year), max(Year), by = 1),
          fill = list(avg_chloride = NA)
        ) |>
        ungroup()

      # --- recycle colors safely if needed ---
      n_series <- n_distinct(df_plot$series)
      color_vals <- rep(okabe_ito, length.out = n_series)
      names(color_vals) <- unique(df_plot$series)

      # --- build plot ---
      p <- ggplot(
        df_plot,
        aes(
          x = Year,
          y = avg_chloride,
          color = series,
          shape = series,
          group = series
        )
      ) +
        geom_line(linewidth = 0.5, na.rm = TRUE) +
        geom_point(size = 2.0) +
        scale_color_manual(values = color_vals) +
        scale_x_continuous(
          breaks = seq(min(df_plot$Year), max(df_plot$Year), by = 1),
          labels = function(x) format(x, nsmall = 0)
        ) +
        scale_y_continuous(
          name = "Chloride (mg/L)",
          expand = expansion(mult = c(0.02, 0.05))
        ) +
        labs(
          title = "Historical Trend Epilimnetic and Inlet Chloride",
          x = "Year"
        ) +
        guides(
          color = guide_legend(title = "Station"),
          shape = guide_legend(title = "Station")
        ) +
        theme_bw() +
        theme_chloride() +
        theme(
          legend.position = "right",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9)
        )

      # --- file name ---
      out_file <- file.path(
        chloride_path,
        paste0(
          gsub(" ", "_", lake),
          "_",
          gsub(" ", "_", town),
          "_chloride.png"
        )
      )

      # --- save plot ---
      ggsave(
        filename = out_file,
        plot = p,
        width = 8,
        height = 4,
        dpi = 300,
        bg = "white"
      )
    }
  )
}
