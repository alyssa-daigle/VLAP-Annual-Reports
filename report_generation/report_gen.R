#load required libraries
library(rmarkdown)
library(dplyr)

#load env file
if (file.exists(".env")) {
  if (!requireNamespace("dotenv", quietly = TRUE)) {
    install.packages("dotenv")
  }
  library(dotenv)
  load_dot_env(".env")
}

#define paths
input_path <- Sys.getenv("INPUT_PATH")
report_path <- Sys.getenv("REPORT_PATH")

#load LAKEMAP
LAKEMAP <- read.csv(input_path, "LAKEMAP.csv", stringsAsFactors = FALSE)

#output directory
output_dir <- report_path
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#loop through each unique station
for (i in seq_len(nrow(LAKEMAP))) {
  lake <- LAKEMAP$RELLAKE[i]
  station <- LAKEMAP$STATNAME[i]
  station_id <- LAKEMAP$STATIONID[i]

  message("Rendering report for: ", lake, " (", station, ")")

  render(
    input = "report_template.Rmd", #report template Rmd
    output_format = "word_document",
    output_file = paste0(station, "_", lake, "_Report.docx"),
    output_dir = output_dir,
    params = list(
      lake = lake,
      station = station,
      station_id = station_id
    ),
    envir = new.env() # isolate each run
  )
}
