make_plankton_monthly <- function(INPUT_PATH, OUTPUT_PATH) {
  # -----------------------------
  # Ensure output directory exists
  # -----------------------------
  monthly_output <- file.path(
    OUTPUT_PATH,
    "plankton_monthly"
  )

  if (!dir.exists(monthly_output)) {
    dir.create(monthly_output, recursive = TRUE)
  }

  library(ggpattern)

  # -----------------------------
  # Load YEAR-driven plankton file
  # -----------------------------
  plankton_file <- file.path(
    INPUT_PATH,
    paste0("Historical_Phytoplankton_Data_Thru", YEAR, ".xlsx")
  )

  data <- read_excel(plankton_file)

  # -----------------------------
  # Processing
  # -----------------------------
  data <- data |>
    mutate(
      year = lubridate::year(`date-time`),
      month = lubridate::month(`date-time`),
      WQDStationID = trimws(as.character(WQDStationID))
    ) |>
    select(-`date-time`)

  # -----------------------------
  # Keep only YEAR data
  # -----------------------------
  data <- data |>
    filter(
      year == YEAR,
      !is.na(WQDStationID),
      WQDStationID != ""
    )

  # -----------------------------
  # Clean COMMENTS
  # -----------------------------
  data <- data |>
    mutate(
      COMMENTS = as.character(COMMENTS),
      COMMENTS = trimws(COMMENTS),
      COMMENTS = toupper(COMMENTS),
      COMMENTS = ifelse(
        is.na(COMMENTS) | COMMENTS == "UNKNOWN PHYTO",
        "OTHER",
        COMMENTS
      )
    ) |>
    filter(
      COMMENTS %in%
        c(
          "GREEN",
          "GOLDEN-BROWN",
          "EUGLENOID",
          "DINOFLAGELLATE",
          "DIATOM",
          "CYANOBACTERIA",
          "CRYPTOMONAD",
          "XANTHOPHYTE",
          "OTHER"
        )
    )

  # -----------------------------
  # Find LAKE + STATION combinations
  # with at least 2 months of data
  # -----------------------------
  stations_with_multiple_months <- data |>
    distinct(lake, WQDStationID, month) |>
    count(
      lake,
      WQDStationID,
      name = "n_months"
    ) |>
    filter(n_months >= 2)

  message(
    "Found ",
    nrow(stations_with_multiple_months),
    " lake and station combinations with at least 2 months of ",
    YEAR,
    " plankton data."
  )

  # -----------------------------
  # Keep only stations with
  # at least 2 months of data
  # -----------------------------
  data <- data |>
    semi_join(
      stations_with_multiple_months,
      by = c("lake", "WQDStationID")
    )

  # -----------------------------
  # Relative abundance by
  # lake + station + year + month
  # -----------------------------
  rel_abund <- data |>
    group_by(
      lake,
      WQDStationID,
      year,
      month,
      COMMENTS
    ) |>
    summarise(
      total_count = sum(Percent, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(
      lake,
      WQDStationID,
      year,
      month
    ) |>
    mutate(
      rel_abundance = total_count / sum(total_count)
    ) |>
    ungroup()

  # -----------------------------
  # Lump groups <3% into OTHER
  # -----------------------------
  rel_abund <- rel_abund |>
    group_by(
      lake,
      WQDStationID,
      year,
      month,
      COMMENTS
    ) |>
    summarise(
      rel_abundance = sum(rel_abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(
      lake,
      WQDStationID,
      year,
      month
    ) |>
    mutate(
      COMMENTS = ifelse(
        rel_abundance < 0.03,
        "OTHER",
        COMMENTS
      )
    ) |>
    group_by(
      lake,
      WQDStationID,
      year,
      month,
      COMMENTS
    ) |>
    summarise(
      rel_abundance = sum(rel_abundance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    ungroup()

  data <- rel_abund

  # -----------------------------
  # Color palette
  # -----------------------------
  algae_colors <- c(
    "GREEN" = "#2E8B3A",
    "GOLDEN-BROWN" = "#D4A017",
    "EUGLENOID" = "#000000",
    "DINOFLAGELLATE" = "#CC79A7",
    "DIATOM" = "#0072B2",
    "CYANOBACTERIA" = "#E8601C",
    "CRYPTOMONAD" = "#56B4E9",
    "XANTHOPHYTE" = "#C2B280",
    "OTHER" = "grey70"
  )

  algae_labels <- c(
    "GREEN" = "Greens",
    "GOLDEN-BROWN" = "Golden-Browns",
    "EUGLENOID" = "Euglenoids",
    "DINOFLAGELLATE" = "Dinoflagellates",
    "DIATOM" = "Diatoms",
    "CYANOBACTERIA" = "Cyanobacteria",
    "CRYPTOMONAD" = "Cryptomonads",
    "XANTHOPHYTE" = "Xanthophytes",
    "OTHER" = "Other"
  )

  # -----------------------------
  # Pattern mappings
  # -----------------------------
  algae_patterns <- c(
    "GREEN" = "none",
    "GOLDEN-BROWN" = "stripe",
    "EUGLENOID" = "none",
    "DINOFLAGELLATE" = "stripe",
    "DIATOM" = "stripe",
    "CYANOBACTERIA" = "circle",
    "CRYPTOMONAD" = "crosshatch",
    "XANTHOPHYTE" = "crosshatch",
    "OTHER" = "stripe"
  )

  algae_pattern_angles <- c(
    "GREEN" = 30,
    "GOLDEN-BROWN" = 90,
    "EUGLENOID" = 90,
    "DINOFLAGELLATE" = 30,
    "DIATOM" = 0,
    "CYANOBACTERIA" = 30,
    "CRYPTOMONAD" = 0,
    "XANTHOPHYTE" = 120,
    "OTHER" = 120
  )

  # -----------------------------
  # Legend order
  # -----------------------------
  legend_order <- c(
    sort(setdiff(names(algae_colors), "OTHER")),
    "OTHER"
  )

  # -----------------------------
  # Month labels
  # -----------------------------
  month_labels <- c(
    "1" = "January",
    "2" = "February",
    "3" = "March",
    "4" = "April",
    "5" = "May",
    "6" = "June",
    "7" = "July",
    "8" = "August",
    "9" = "September",
    "10" = "October",
    "11" = "November",
    "12" = "December"
  )

  # -----------------------------
  # Get list of lake + station
  # combinations
  # -----------------------------
  plot_keys <- data |>
    distinct(lake, WQDStationID) |>
    arrange(lake, WQDStationID)

  message(
    "Generating monthly plots for ",
    nrow(plot_keys),
    " lake and station combinations."
  )

  # -----------------------------
  # Generate one plot per station
  # -----------------------------
  lapply(seq_len(nrow(plot_keys)), function(i) {
    lake_name <- plot_keys$lake[i]
    station_id <- plot_keys$WQDStationID[i]

    message(
      paste0(
        "Working on monthly plankton for ",
        lake_name,
        " | Station: ",
        station_id,
        "\n"
      )
    )

    # -----------------------------
    # Subset lake + station
    # -----------------------------
    plot_data <- data |>
      filter(
        .data$lake == .env$lake_name,
        .data$WQDStationID == .env$station_id
      )

    # -----------------------------
    # Get months with data
    # -----------------------------
    months_with_data <- sort(
      unique(plot_data$month)
    )

    # -----------------------------
    # Skip if fewer than 2 months
    # -----------------------------
    if (length(months_with_data) < 2) {
      message(
        "  -> Skipping ",
        station_id,
        " (fewer than 2 months of ",
        YEAR,
        " data)\n"
      )

      return(NULL)
    }

    # -----------------------------
    # Complete missing group
    # combinations within each month
    # -----------------------------
    plot_data <- plot_data |>
      mutate(
        COMMENTS = factor(
          COMMENTS,
          levels = legend_order
        ),
        month = factor(
          month,
          levels = months_with_data,
          labels = month_labels[
            as.character(months_with_data)
          ]
        )
      ) |>
      tidyr::complete(
        month = levels(month),
        COMMENTS = legend_order,
        fill = list(
          rel_abundance = 0
        )
      ) |>
      mutate(
        COMMENTS = factor(
          COMMENTS,
          levels = legend_order
        ),
        month = factor(
          month,
          levels = month_labels[
            as.character(months_with_data)
          ]
        )
      )

    # -----------------------------
    # Main plot
    # -----------------------------
    p_main <- ggplot(
      plot_data,
      aes(
        x = month,
        y = rel_abundance,
        fill = COMMENTS,
        pattern = COMMENTS,
        pattern_angle = COMMENTS
      )
    ) +

      geom_bar_pattern(
        stat = "identity",
        position = "stack",

        color = "white",
        linewidth = 0.05,

        pattern_fill = "grey25",
        pattern_colour = NA,
        pattern_density = 0.12,
        pattern_spacing = 0.04,
        pattern_key_scale_factor = 1.8
      ) +

      scale_y_continuous(
        labels = scales::percent,
        breaks = seq(0, 1, by = 0.1),
        expand = c(0, 0),
        limits = c(0, 1)
      ) +

      scale_fill_manual(
        values = algae_colors,
        labels = algae_labels,
        drop = FALSE
      ) +

      scale_pattern_manual(
        values = algae_patterns,
        drop = FALSE
      ) +

      scale_pattern_angle_manual(
        values = algae_pattern_angles,
        drop = FALSE
      ) +

      guides(
        fill = guide_legend(
          title = "Group",
          ncol = 1,
          byrow = TRUE,
          override.aes = {
            levs <- legend_order[
              legend_order %in% names(algae_colors)
            ]

            list(
              pattern = algae_patterns[levs],
              pattern_angle = algae_pattern_angles[levs],
              pattern_fill = "grey25",
              pattern_colour = NA,
              pattern_density = 0.12,
              pattern_spacing = 0.01,
              pattern_key_scale_factor = 1.8
            )
          }
        ),

        pattern = "none",
        pattern_angle = "none"
      ) +

      labs(
        title = paste0(
          lake_name,
          " Monthly Phytoplankton Population - ",
          YEAR
        ),
        subtitle = paste0(
          "Station: ",
          station_id
        ),
        x = "Month",
        y = "Relative Abundance",
        fill = NULL
      ) +

      theme_bw() +
      theme_plankton() +

      theme(
        legend.title = element_text(
          family = "Calibri",
          face = "bold"
        )
      )

    # -----------------------------
    # Save using WQDStationID
    # -----------------------------
    filename <- paste0(
      station_id,
      "_monthly_plankton.png"
    )

    filename <- gsub(
      "[^A-Za-z0-9_.-]",
      "_",
      filename
    )

    temp_path <- file.path(
      monthly_output,
      filename
    )

    ggsave(
      temp_path,
      plot = p_main,
      width = 8,
      height = 4,
      dpi = 300,
      bg = "white"
    )

    # -----------------------------
    # Add border
    # -----------------------------
    img <- magick::image_read(temp_path)

    img_bordered <- magick::image_border(
      img,
      color = "black",
      geometry = "7x7"
    )

    magick::image_write(
      img_bordered,
      path = temp_path,
      format = "png"
    )
  })
}
