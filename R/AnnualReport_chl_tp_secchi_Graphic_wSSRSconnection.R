# Load required libraries
library(httr) # For making HTTP requests (to fetch the SSRS report)
library(readr) # For reading CSV files into R
library(dplyr) # For data manipulation
library(tidyr) # For reshaping data (pivot_wider)

# ---------------------------
# Step 1: Define the SSRS Report URL
# ---------------------------
url <- "https://ssrsportal.des.nh.gov/ReportServer?/Environmental%20Services/Water%20Division/Watershed%20Bureau/VLAP/Regression&rs:Format=CSV"
# Note: rs:Format=CSV tells SSRS to return raw CSV output instead of HTML

# ---------------------------
# Step 2: Fetch the report using Windows authentication
# ---------------------------
# Load environment variables from .env file
if (file.exists(".env")) {
  readRenviron(".env")
}

# Use the loaded environment variables for authentication
res <- GET(
  url,
  authenticate(
    Sys.getenv("USERNAME"),
    Sys.getenv("PASSWORD"),
    type = "ntlm"
  )
)
# NTLM authentication allows R to use Windows credentials to access the protected report
# Sys.getenv() reads the credentials from environment variables (more secure than hardcoding)

# ---------------------------
# Step 3: Read CSV content from the response into a tibble
# ---------------------------
data <- read_csv(content(res, "raw"))
# 'content(res, "raw")' extracts the raw CSV bytes from the GET response

# ---------------------------
# Step 4: Select only relevant columns from the raw SSRS export
# ---------------------------
df_clean <- data |>
  select(
    RELLAKE,
    TOWN,
    STATIONID,
    STATNAME,
    YEAR1,
    ParameterEdited,
    DepthZoneEdited,
    NUMRESULT
  )

# ---------------------------
# Step 5: Map each parameter + depth combination to your desired column names
# ---------------------------
df_clean <- df_clean |>
  mutate(
    Parameter_Depth = case_when(
      ParameterEdited == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" &
        DepthZoneEdited == "COMPOSITE" ~
        "CHL_comp",
      ParameterEdited ==
        "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN
" &
        DepthZoneEdited == "EPILIMNION" ~
        "CHL_epi",
      ParameterEdited == "SPECIFIC CONDUCTANCE" &
        DepthZoneEdited == "EPILIMNION" ~
        "SPCD_epi",
      ParameterEdited == "PH" & DepthZoneEdited == "EPILIMNION" ~ "PH_epi",
      ParameterEdited == "PHOSPHORUS AS P" & DepthZoneEdited == "EPILIMNION" ~
        "TP_epi",
      ParameterEdited == "PHOSPHORUS AS P" & DepthZoneEdited == "HYPOLIMNION" ~
        "TP_hypo",
      ParameterEdited == "SECCHI DISK TRANSPARENCY" ~ "SECCHI",
      ParameterEdited == "Secchi-Scope" ~ "Secchi_Scope",
    )
  ) |>
  filter(!is.na(Parameter_Depth)) # Keep only rows that we mapped

# ---------------------------
# Step 6: Pivot the data from long to wide format
# ---------------------------
df_wide <- df_clean |>
  select(
    RELLAKE,
    TOWN,
    STATIONID,
    STATNAME,
    YEAR1,
    Parameter_Depth,
    NUMRESULT
  ) |>
  pivot_wider(
    names_from = Parameter_Depth, # Column names come from Parameter_Depth
    values_from = NUMRESULT # Values come from NUMRESULT
  )

# ---------------------------
# Step 7: Rename columns to match your desired format
# ---------------------------
df_wide <- df_wide |>
  rename(
    Rel_Lake = RELLAKE,
    Town = TOWN,
    StationID = STATIONID,
    Station_Name = STATNAME,
    Year = YEAR1,
    `Secchi-Scope` = Secchi_Scope
  )

BTC <- read_excel(
  "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Reporting/Annual reports/2025/AnnualReport_Scripts/BTC.xlsx"
)

# ---------------------------
# Remove duplicate rows by RELLAKE
# ---------------------------
BTC <- BTC |>
  distinct(RELLAKE, .keep_all = TRUE)

# ---------------------------
# Create numeric thresholds for each trophic class
# ---------------------------
trophic_thresholds <- tibble(
  BEST_TROPHIC_CLASS = c("EUTROPHIC", "MESOTROPHIC", "OLIGOTROPHIC"),
  CHLa_thresh = c(11, 5, 3),
  TP_thresh = c(28, 12, 8)
)

# ---------------------------
# Merge BTC into data
# ---------------------------
data <- df_wide |>
  left_join(BTC, by = c("Rel_Lake" = "RELLAKE")) |> # adds BEST_TROPHIC_CLASS
  left_join(trophic_thresholds, by = "BEST_TROPHIC_CLASS") # adds CHLa_thresh & TP_thresh

# ---------------------------
# Output folder for plots
# ---------------------------
output_folder <- "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Reporting/Annual reports/2025/AnnualReport_Scripts/TP_CHL_SECCHI_plots"
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

  # Filter data for this StationID
  df_plot <- data |>
    filter(StationID == station_id) |>
    filter(!is.na(CHL_comp) | !is.na(TP_epi) | !is.na(SECCHI)) |>
    arrange(Year)

  if (nrow(df_plot) == 0) {
    next
  }
  if (!any(df_plot$Year == 2025)) {
    next
  } # Only plot if there is 2025 data

  # ---------------------------
  # Extract thresholds for this lake
  # ---------------------------
  chl_thresh <- unique(df_plot$CHLa_thresh)
  tp_thresh <- unique(df_plot$TP_thresh)

  # ---------------------------
  # Determine first and last year with data
  # ---------------------------
  first_year <- min(df_plot$Year, na.rm = TRUE)
  last_year <- max(df_plot$Year, na.rm = TRUE)
  all_years <- first_year:last_year

  # ---------------------------
  # Fill in missing years
  # ---------------------------
  df_plot <- df_plot |>
    complete(
      Year = all_years,
      fill = list(TP_epi = NA, CHL_comp = NA, SECCHI = NA)
    ) |>
    mutate(Year = factor(Year, levels = all_years))

  # ---------------------------
  # Determine maximum values for scaling axes
  # ---------------------------
  max_left <- max(c(df_plot$CHL_comp, df_plot$TP_epi), na.rm = TRUE)
  max_right <- max(df_plot$SECCHI, na.rm = TRUE)

  if (!is.finite(max_left) & !is.finite(max_right)) {
    next
  }
  if (max_left == 0 & max_right == 0) {
    next
  }

  scale_factor <- ifelse(max_right > 0, max_left / max_right, 1)
  y_max_left <- max_left * 1.75
  y_max_right <- max_right * 1.75

  # ---------------------------
  # Flip Secchi bars
  # ---------------------------
  df_plot <- df_plot |>
    mutate(
      secchi_top = pmin(y_max_left, y_max_right * scale_factor),
      secchi_bottom = pmax(0, (y_max_right - SECCHI) * scale_factor)
    )

  # ---------------------------
  # Build the plot
  # ---------------------------
  p <- ggplot(df_plot, aes(x = Year)) +

    # Secchi bars
    geom_rect(
      aes(
        xmin = as.numeric(Year) - 0.3,
        xmax = as.numeric(Year) + 0.3,
        ymin = secchi_bottom,
        ymax = secchi_top
      ),
      fill = "lightsteelblue2",
      color = "gray20"
    ) +

    # TP line & points
    {
      if (sum(!is.na(df_plot$TP_epi)) > 1) {
        geom_line(aes(y = TP_epi, group = 1), color = "red4", size = 0.5)
      }
    } +
    geom_point(aes(y = TP_epi), color = "red4", shape = 17, size = 2) +

    # Chl line & points
    {
      if (sum(!is.na(df_plot$CHL_comp)) > 1) {
        geom_line(
          aes(y = CHL_comp, group = 1),
          color = "springgreen4",
          size = 0.5
        )
      }
    } +
    geom_point(
      aes(y = CHL_comp),
      color = "springgreen4",
      shape = 16,
      size = 2
    ) +

    # Add threshold lines if they exist
    {
      if (!is.na(chl_thresh)) {
        geom_hline(
          yintercept = chl_thresh,
          linetype = "dashed",
          color = "springgreen4",
          size = 0.8
        )
      }
    } +
    {
      if (!is.na(tp_thresh)) {
        geom_hline(
          yintercept = tp_thresh,
          linetype = "dashed",
          color = "red4",
          size = 0.8
        )
      }
    } +

    # Titles and X-axis
    labs(title = paste("Station", station_id), x = "Year") +

    # Theme
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # ---------------------------
  # Y-axis with secondary Secchi axis
  # ---------------------------
  if (sum(!is.na(df_plot$SECCHI)) > 1) {
    p <- p +
      scale_y_continuous(
        name = "TP & Chl (µg/L)",
        limits = c(0, y_max_left),
        expand = c(0, 0),
        breaks = seq(0, ceiling(y_max_left * 2) / 2, by = 5.0),
        labels = function(x) sprintf("%.1f", x),
        sec.axis = sec_axis(
          trans = ~ y_max_right - (. / scale_factor),
          name = "Secchi (m)",
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
  # Safe filename (no "Station_" prefix)
  # ---------------------------
  filename <- paste0(
    gsub("[^A-Za-z0-9]", "_", station_id),
    "_TP_CHL_SECCHI.png"
  )

  # ---------------------------
  # Save plot
  # ---------------------------
  ggsave(
    file.path(output_folder, filename),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
}
