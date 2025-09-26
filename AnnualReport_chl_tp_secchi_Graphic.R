library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(tibble)

# ---------------------------
# Load the data
# ---------------------------
data <- read_excel(
  "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Reporting/Annual reports/2025/AnnualReport_Scripts/pH_conduc_historical.xlsx"
)
BTC <- read_excel(
  "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Reporting/Annual reports/2025/AnnualReport_Scripts/BTC.xlsx"
)

BTC <- BTC %>% distinct(RELLAKE, .keep_all = TRUE)

trophic_thresholds <- tibble(
  BEST_TROPHIC_CLASS = c("EUTROPHIC", "MESOTROPHIC", "OLIGOTROPHIC"),
  CHLa_thresh = c(11, 5, 3),
  TP_thresh = c(28, 12, 8)
)

data <- data %>%
  left_join(BTC, by = c("Rel_Lake" = "RELLAKE")) %>%
  left_join(trophic_thresholds, by = "BEST_TROPHIC_CLASS")

output_folder <- "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Reporting/Annual reports/2025/AnnualReport_Scripts/TP_CHL_SECCHI_plots"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

station_list <- data %>% select(StationID) %>% distinct() %>% arrange(StationID)

# ---------------------------
# Loop through each StationID
# ---------------------------
for (i in 1:nrow(station_list)) {
  station_id <- station_list$StationID[i]

  df_plot <- data %>%
    filter(StationID == station_id) %>%
    filter(!is.na(CHL_comp) | !is.na(TP_epi) | !is.na(SECCHI)) %>%
    arrange(Year)

  if (nrow(df_plot) == 0) {
    next
  }
  if (!any(df_plot$Year == 2025)) {
    next
  }

  chl_thresh <- unique(df_plot$CHLa_thresh)
  tp_thresh <- unique(df_plot$TP_thresh)

  first_year <- min(df_plot$Year, na.rm = TRUE)
  last_year <- max(df_plot$Year, na.rm = TRUE)
  all_years <- first_year:last_year

  df_plot <- df_plot %>%
    complete(
      Year = all_years,
      fill = list(TP_epi = NA, CHL_comp = NA, SECCHI = NA)
    ) %>%
    mutate(Year = factor(Year, levels = all_years))

  # ---------------------------
  # Create legend mapping variables
  # ---------------------------
  df_plot <- df_plot %>%
    mutate(
      SECCHI_leg = "Transparency (m)",
      TP_leg = "Phosphorus (µg/L)",
      CHL_leg = "Chlorophyll-a (µg/L)",
      TP_thresh_leg = ifelse(!is.na(tp_thresh), "Phos. BTC Threshold", NA),
      CHL_thresh_leg = ifelse(!is.na(chl_thresh), "Chl-a BTC Threshold", NA)
    )

  # Scaling for Secchi bars
  max_left <- max(c(df_plot$CHL_comp, df_plot$TP_epi), na.rm = TRUE)
  max_right <- max(df_plot$SECCHI, na.rm = TRUE)
  scale_factor <- ifelse(max_right > 0, max_left / max_right, 1)
  y_max_left <- max_left * 1.5
  y_max_right <- max_right * 1.5

  df_plot <- df_plot %>%
    mutate(
      secchi_top = pmin(y_max_left, y_max_right * scale_factor),
      secchi_bottom = pmax(0, (y_max_right - SECCHI) * scale_factor)
    )

  # ---------------------------
  # Build plot
  # ---------------------------
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

    # TP line & points
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

    # Chl line & points
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

    # Threshold lines (only one key per type)
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

    # Labels
    labs(
      title = "Historical Chlorophyll-a, Epilimnetic Phosphorus, \n& Transparency Data",
      x = "Year",
      fill = NULL,
      color = NULL
    ) +

    # Manual scales
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
      values = c(
        "TP Threshold" = "dashed",
        "Chl-a Threshold" = "dashed"
      )
    ) +

    # Guides for points (no line through legend)
    guides(
      color = guide_legend(override.aes = list(size = 2, linetype = 0)),
      fill = guide_legend(
        override.aes = list(shape = 22, size = 5, color = "black")
      )
    ) +

    # Theme
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = c(1.0, 1.1),
      legend.background = element_rect(fill = "transparent", size = 0.5),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
      legend.text = element_text(size = 8),
      legend.key.width = unit(0.3, "cm"),
      legend.key.height = unit(0.3, "cm"),
      plot.margin = margin(t = 35, r = 40, b = 10, l = 20),
      plot.title = element_text(
        hjust = 0.05,
        face = "bold",
        margin = margin(b = 20)
      ),
      axis.line = element_line(color = "gray40")
    )

  # ---------------------------
  # Y-axis with secondary Secchi axis
  # ---------------------------
  if (sum(!is.na(df_plot$SECCHI)) > 1) {
    p <- p +
      scale_y_continuous(
        name = "Chlorophyll-a & Total Phosphorus (µg/L)",
        limits = c(0, y_max_left),
        expand = c(0, 0),
        breaks = seq(0, ceiling(y_max_left * 2) / 2, by = 5.0),
        labels = function(x) sprintf("%.1f", x),
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

  # ---------------------------
  # Save plot
  # ---------------------------
  filename <- paste0(
    gsub("[^A-Za-z0-9]", "_", station_id),
    "_TP_CHL_SECCHI.png"
  )
  ggsave(
    file.path(output_folder, filename),
    plot = p,
    width = 7,
    height = 4,
    dpi = 300
  )
}
