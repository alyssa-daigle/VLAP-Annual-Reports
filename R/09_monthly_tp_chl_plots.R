library(tidyverse)

# ---------------------------------------------------------
# Monthly TP + Tributary plots
# One plot per 6-character STATIONID suffix
# 2026 only
# ---------------------------------------------------------

# ---------------------------------------------------------
# Prepare Total Phosphorus data
# ---------------------------------------------------------

tp_data <- data_monthly %>%
  filter(
    year == 2026,
    WSHEDPARMNAME == "PHOSPHORUS AS P",
    !is.na(NUMRESULT)
  ) %>%
  mutate(
    # First 6 characters of STATIONID
    STATION_SUFFIX = case_when(
      # -------------------------------------------------------
      # COBWIN
      # North deep spot + all COBWIN tributaries
      # South deep spot remains separate
      # -------------------------------------------------------

      STATIONID == "COBWINND" ~ "COBWINN",

      STATIONID == "COBWINSD" ~ "COBWINS",

      str_detect(
        STATIONID,
        "^COBWIN"
      ) ~ "COBWINN",

      # -------------------------------------------------------
      # PEA lakes
      # Deep spots and all associated tributaries use
      # the first 7 characters
      # -------------------------------------------------------

      str_detect(
        STATIONID,
        "^PEABMAD"
      ) ~ "PEABMAD",

      str_detect(
        STATIONID,
        "^PEAMMAD"
      ) ~ "PEAMMAD",

      # -------------------------------------------------------
      # Other special deep spots that need 7 characters
      # -------------------------------------------------------

      STATIONID %in%
        c(
          "PAWNOTND",
          "PAWNOTSD",
          "WINPLACD",
          "WINTLACD",
          "WINMBELD",
          "WINMTILD"
        ) ~ str_sub(
        STATIONID,
        1,
        7
      ),

      # -------------------------------------------------------
      # Default
      # Use first 6 characters
      # -------------------------------------------------------

      TRUE ~ str_sub(
        STATIONID,
        1,
        6
      )
    ),
    # Calendar month order
    month_name = factor(
      month_name,
      levels = c(
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
      )
    ),

    # Identify type of TP observation
    TP_TYPE = case_when(
      # Tributary TP
      str_detect(
        param_depth,
        regex("TP_trib", ignore_case = TRUE)
      ) ~ "Tributary",

      # Regular lake TP
      DEPTHZONE == "EPILIMNION" ~ "Epilimnion",

      DEPTHZONE == "HYPOLIMNION" ~ "Hypolimnion",

      TRUE ~ NA_character_
    ),

    # -------------------------------------------------------
    # Clean tributary station name
    #
    # Example:
    # "LONG POND-INLET"       -> "Inlet"
    # "LONG POND-EAST BRANCH" -> "East Branch"
    # -------------------------------------------------------

    TRIB_NAME = if_else(
      TP_TYPE == "Tributary",
      str_to_title(
        str_trim(
          str_replace(
            STATNAM,
            "^[^-]*-",
            ""
          )
        )
      ),
      NA_character_
    )
  ) %>%

  # Keep only observations we are plotting
  filter(
    !is.na(TP_TYPE)
  )


# ---------------------------------------------------------
# Summarize TP data
#
# The same dataframe contains:
#   - Epilimnion
#   - Hypolimnion
#   - All TP_trib observations
# ---------------------------------------------------------

tp_data <- tp_data %>%
  group_by(
    STATION_SUFFIX,
    month,
    month_name,
    TP_TYPE,
    STATNAM,
    TRIB_NAME
  ) %>%
  summarise(
    TP = median(
      NUMRESULT,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ---------------------------------------------------------
# Get unique 6-character station suffixes
# ---------------------------------------------------------

stations <- tp_data %>%
  distinct(
    STATION_SUFFIX
  ) %>%
  arrange(
    STATION_SUFFIX
  )


# ---------------------------------------------------------
# Output folder
# ---------------------------------------------------------

plot_dir <- file.path(
  OUTPUT_BASE,
  "2026",
  "plots",
  "monthly"
)

dir.create(
  plot_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ---------------------------------------------------------
# Generate one plot for each station suffix
# ---------------------------------------------------------

for (i in seq_len(nrow(stations))) {
  station <- stations$STATION_SUFFIX[i]

  # -------------------------------------------------------
  # Get all TP data for this station suffix
  # -------------------------------------------------------

  station_data <- tp_data %>%
    filter(
      STATION_SUFFIX == station
    )

  # -------------------------------------------------------
  # Count number of months with data
  # -------------------------------------------------------

  n_months <- station_data %>%
    distinct(
      month
    ) %>%
    nrow()

  # Skip stations with only one month of data

  if (n_months < 2) {
    next
  }

  # -------------------------------------------------------
  # Separate bars and tributary lines
  # from the SAME dataframe
  # -------------------------------------------------------

  tp_bars <- station_data %>%
    filter(
      TP_TYPE %in%
        c(
          "Epilimnion",
          "Hypolimnion"
        )
    )

  tp_trib <- station_data %>%
    filter(
      TP_TYPE == "Tributary"
    )

  # -------------------------------------------------------
  # Get lake/station name for title
  # -------------------------------------------------------

  station_name <- station_data %>%
    filter(
      TP_TYPE %in%
        c(
          "Epilimnion",
          "Hypolimnion"
        ),
      !is.na(STATNAM)
    ) %>%
    pull(
      STATNAM
    ) %>%
    first()

  # -------------------------------------------------------
  # Create plot
  # -------------------------------------------------------

  p <- ggplot() +

    # -----------------------------------------------------
    # Epilimnion + Hypolimnion TP bars
    # -----------------------------------------------------

    geom_col(
      data = tp_bars,
      aes(
        x = month_name,
        y = TP,
        fill = TP_TYPE
      ),
      position = position_dodge(
        width = 0.8
      ),
      width = 0.7
    ) +

    # -----------------------------------------------------
    # All TP tributary lines
    #
    # Each tributary gets its own:
    #   - color
    #   - line type
    #   - group
    # -----------------------------------------------------

    geom_line(
      data = tp_trib,
      aes(
        x = month_name,
        y = TP,
        group = TRIB_NAME,
        color = TRIB_NAME,
        linetype = TRIB_NAME
      ),
      linewidth = 1
    ) +

    # -----------------------------------------------------
    # Tributary points
    # -----------------------------------------------------

    geom_point(
      data = tp_trib,
      aes(
        x = month_name,
        y = TP,
        color = TRIB_NAME,
        shape = TRIB_NAME
      ),
      size = 4
    ) +

    # -----------------------------------------------------
    # Single y-axis
    # -----------------------------------------------------

    scale_y_continuous(
      name = "Total Phosphorus (µg/L)"
    ) +

    scale_fill_manual(
      values = c(
        "Epilimnion" = "gray50",
        "Hypolimnion" = "gray80"
      )
    ) +
    # -----------------------------------------------------
    # Labels
    # -----------------------------------------------------

    labs(
      title = "Monthly Total Phosphorus Levels: Deep Spot and Tributaries",

      x = NULL,

      fill = "Deep Spot Depth Zone",

      color = "Tributary",

      linetype = "Tributary",

      shape = "Tributary"
    ) +

    # -----------------------------------------------------
    # Theme
    # -----------------------------------------------------

    theme_minimal() +

    theme(
      plot.title = element_text(
        face = "bold"
      ),

      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),

      legend.position = "right"
    ) +
    theme_bw()

  # -------------------------------------------------------
  # Save plot
  # -------------------------------------------------------

  ggsave(
    filename = file.path(
      plot_dir,
      paste0(
        station,
        "_monthly_2026.png"
      )
    ),

    plot = p,

    width = 10,
    height = 6,
    dpi = 300
  )
}
