# VLAP Annual Reports Generator - by Station

library(magick) # for image reading and manipulation
library(gridExtra) # for arranging multiple plots
library(grid) # for rasterGrob

# define directories with plots
dirs <- c(
  "/Users/alyssadaigle/Desktop/VLAP-Annual-Reports/plots/chl_tp_secchi",
  "/Users/alyssadaigle/Desktop/VLAP-Annual-Reports/plots/pH_conduc",
  "/Users/alyssadaigle/Desktop/VLAP-Annual-Reports/plots/temp_DO/",
  "/Users/alyssadaigle/Desktop/VLAP-Annual-Reports/plots/plankton/"
)

# output directory with reports (update yearly)
output_dir <- "/Users/alyssadaigle/Desktop/VLAP-Annual-Reports/reports_2025/"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# station list
# eventually will have a whole DF that will include lake_name, town_name, stationID so we can also plug in lake and town name
stations <- c("StationA", "StationB", "StationC")

# report generation function

generate_station_report <- function(station, dirs, output_dir) {
  # build file paths for 4 plots
  plots <- sapply(dirs, function(d) {
    file.path(d, paste0(station, ".jpg"))
  })

  # check for missing plots
  missing <- !file.exists(plots)
  if (any(missing)) {
    warning(paste(
      "Missing plots for station",
      station,
      ":",
      paste(plots[missing], collapse = ", ")
    ))
    # replace missing plots with a blank placeholder
    plots[missing] <- NA
  }

  # read plots
  img_list <- lapply(plots, function(p) {
    if (is.na(p)) {
      # create blank white image as placeholder
      image_blank(width = 800, height = 600, color = "white")
    } else {
      image_read(p)
    }
  })

  # arrange 4 plots in 2x2 grid
  top_row <- image_append(c(img_list[[1]], img_list[[2]]), stack = FALSE)
  bottom_row <- image_append(c(img_list[[3]], img_list[[4]]), stack = FALSE)
  combined <- image_append(c(top_row, bottom_row), stack = TRUE)

  # save combined image as PDF
  output_file <- file.path(output_dir, paste0(station, "_report.pdf"))
  image_write(combined, path = output_file, format = "pdf")

  message(paste("Report saved for station:", station))
}

# generate reports for all stations
for (st in stations) {
  generate_station_report(st, dirs, output_dir)
}
