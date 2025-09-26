library(tidyr)
library(ggplot2)
library(readxl)
library(dplyr)

#load data
data <- read_excel(
  "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Management/DO EMD Upload/2025/master-DO-2025.xlsm"
)

#clean and prepare
temp_do <- data %>%
  select(Depth, Date, Time, Station, DO, Temp...12) %>%
  rename(Temp = `Temp...12`) %>%
  mutate(
    Date = as.Date(Date),
    Month = format(Date, "%B"),
    Month = factor(Month, levels = month.name, ordered = TRUE)
  )

#get list of unique station names
stations <- unique(temp_do$Station)

#loop through each station and make a temp/DO plot
for (stn in stations) {
  temp_do_stn <- temp_do %>%
    filter(Station == stn) %>%
    select(Depth, Date, Month, Time, Station, DO, Temp)

  #average duplicate profiles, if they exist
  temp_do_avg <- temp_do_stn %>%
    group_by(Date, Depth) %>%
    summarise(
      Temp = mean(Temp, na.rm = TRUE),
      DO = mean(DO, na.rm = TRUE),
      Month = first(Month),
      .groups = "drop"
    )

  #reshape
  temp_do_long <- temp_do_avg %>%
    pivot_longer(cols = c(Temp, DO), names_to = "Variable", values_to = "Value")

  #compute max depth for this station
  max_depth <- max(temp_do_long$Depth, na.rm = TRUE)

  #generate the plot
  p <- ggplot(
    temp_do_long,
    aes(
      x = Value,
      y = Depth,
      color = Variable,
      shape = Month,
      group = interaction(Date, Variable)
    )
  ) +
    geom_path() +
    geom_point(size = 2) +
    scale_y_reverse(breaks = seq(0, ceiling(max_depth), by = 1)) +
    scale_color_manual(values = c("Temp" = "darkblue", "DO" = "darkgreen")) +
    labs(
      title = "Dissolved Oxygen & Temperature Profiles 2025",
      x = "Temperature (°C) and Dissolved Oxygen (mg/L)",
      y = "Depth (m)",
      color = "Parameter",
      shape = "Month"
    ) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "whitesmoke"),
      panel.grid.minor.y = element_blank()
    )

  #save jpg for each station
  ggsave(
    filename = paste0(
      "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Reporting/Annual reports/2025/AnnualReport_Scripts/Profiles/",
      stn,
      "_profile.jpg"
    ),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300 #adjust depending on report template needs
  )

  message("Saved plot for station: ", stn)
}
