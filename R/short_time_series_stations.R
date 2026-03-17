#stations to address
short_stations <- c("MILSALD", "OPELACD", "SILHLSD", "WEAWEAD")

short_data <- data_plot |>
  filter(STATIONID %in% short_stations)

# Output path for CSV
output_csv <- "short_data.csv" # or full path if you want a specific folder

# Write to CSV
write.csv(short_data, output_csv, row.names = FALSE)
