make_chloride <- function(input_path, output_path) {
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

      message("working on chloride for ", lake, " (", town, ")\n")

      # --- average chloride per station per year ---
      df_plot <- cya_cl |>
        filter(RELLAKE == lake, TOWN == town) |>
        group_by(STATIONID, STATNAME, Year) |>
        summarise(
          avg_chloride = mean(avg_result, na.rm = TRUE),
          .groups = "drop"
        ) |>
        # keep only Epilimnion or stations with "inlet" in their name
        filter(
          STATNAME == "Epilimnion" | str_detect(tolower(STATNAME), "inlet")
        ) |>
        arrange(Year)

      # --- skip if no usable data ---
      if (nrow(df_plot) == 0) {
        return(NULL)
      }

      # --- define line label ---
      df_plot <- df_plot |>
        mutate(series = STATNAME)

      # --- build plot ---
      p <- ggplot(
        df_plot,
        aes(x = Year, y = avg_chloride, color = series, group = series)
      ) +
        geom_line(linewidth = 0.7, na.rm = TRUE, na.exclude = TRUE) +
        geom_point(size = 1.6) +
        scale_y_continuous(
          name = "Chloride (mg/L)",
          expand = expansion(mult = c(0.02, 0.05))
        ) +
        labs(
          title = "Historical Trend Epilimnetic and Inlet Chloride",
          x = "Year",
          color = "Station"
        ) +
        theme_bw() +
        theme_chloride() +
        theme(
          legend.position = "right"
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
