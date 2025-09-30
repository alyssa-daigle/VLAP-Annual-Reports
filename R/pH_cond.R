library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# ---------------------------
# Load the data
# ---------------------------
data <- read_excel(
  "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Reporting/Annual reports/2025/AnnualReport_Scripts/pH_conduc_historical.xlsx"
)

# ---------------------------
# Output folder for plots
# ---------------------------
output_folder <- "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Reporting/Annual reports/2025/AnnualReport_Scripts/pH_conduc_plots_historical"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---------------------------
# Get all unique StationIDs
# ---------------------------
station_list <- data |>
  select(StationID) |>
  distinct() |>
  arrange(StationID)

# ---------------------------
# Loop through each StationID
# ---------------------------
for (i in 1:nrow(station_list)) {
  station_id <- station_list$StationID[i]

  # Filter data for this station
  df_plot <- data |>
    filter(StationID == station_id) |>
    filter(!is.na(PH_epi) | !is.na(SPCD_epi)) |>
    arrange(Year)

  if (nrow(df_plot) == 0) {
    next
  }
  if (!any(df_plot$Year == 2025)) {
    next
  }

  # Fill in missing years
  first_year <- min(df_plot$Year, na.rm = TRUE)
  last_year <- max(df_plot$Year, na.rm = TRUE)
  all_years <- first_year:last_year

  df_plot <- df_plot |>
    complete(Year = all_years, fill = list(PH_epi = NA, SPCD_epi = NA)) |>
    mutate(Year = factor(Year, levels = all_years))

  # Determine pH + conductivity ranges
  min_pH <- min(df_plot$PH_epi, na.rm = TRUE)
  max_pH <- max(df_plot$PH_epi, na.rm = TRUE)
  max_cond <- max(df_plot$SPCD_epi, na.rm = TRUE)

  if (!is.finite(min_pH) | !is.finite(max_pH) | !is.finite(max_cond)) {
    next
  }

  min_pH <- floor(min_pH * 10) / 10 - 0.2
  max_pH <- ceiling(max_pH * 10) / 10 + 0.2
  scale_factor <- (max_cond) / (max_pH - min_pH)
  y_max_left <- max_cond * 1.1
  pH_range <- c(min_pH, max_pH)

  # Build plot
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
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = c(-0.05, 1.2),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.01, "cm"), # less vertical spacing in legend
      legend.margin = margin(t = 2, r = 2, b = 2, l = 2), # smaller legend margin
      legend.text = element_text(size = 8),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 20), # shrink plot margins
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 20)
      ),
      panel.border = element_blank(), # removes all panel borders
      axis.line = element_line(color = "gray40")
    ) +

    scale_fill_manual(values = c("pH" = "white")) +
    scale_color_manual(values = c("cond" = "red3")) +

    guides(
      fill = guide_legend(override.aes = list(size = 0.5)), # for bar fill (not always precise, key width helps)
      color = guide_legend(override.aes = list(size = 1)) # for cond points
    ) +
    theme(
      legend.key.width = unit(0.25, "cm"),
      legend.key.height = unit(0.25, "cm")
    )

  # Dual y-axis
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

  filename <- paste0(
    gsub("[^A-Za-z0-9]", "_", station_id),
    "_pH_Conductivity.png"
  )

  ggsave(
    file.path(output_folder, filename),
    plot = p,
    width = 7,
    height = 4,
    dpi = 300
  )
}
