# Load required libraries
library(rmarkdown)
library(dplyr)

# Load env file
if (file.exists(".env")) {
  if (!requireNamespace("dotenv", quietly = TRUE)) {
    install.packages("dotenv")
  }
  library(dotenv)
  load_dot_env(".env")
}

# Define paths
input_path <- Sys.getenv("INPUT_PATH")
report_path <- Sys.getenv("REPORT_PATH")

# Load LAKEMAP
LAKEMAP <- read.csv(
  file = file.path(input_path, "LAKEMAP.csv"),
  stringsAsFactors = FALSE
)

# Filter for DEEP stations and keep unique lake–station combos
LAKEMAP_filtered <- LAKEMAP |>
  filter(grepl("DEEP", STATNAME, ignore.case = TRUE)) |>
  distinct(RELLAKE, STATNAME, STATIONID, .keep_all = TRUE)

# Define output directory
output_dir <- report_path
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
} else {
  message("Output directory already exists: ", output_dir)
}

# Define template path
template_path <- "S:/WD-Watershed/Monitoring/Volunteer/VLAP/Data Reporting/Annual reports/2025/VLAP-Annual-Reports/report_generation/report_template.Rmd"

# Loop through each unique DEEP station
for (i in seq_len(nrow(LAKEMAP_filtered))) {
  lake <- LAKEMAP_filtered$RELLAKE[i]
  station <- LAKEMAP_filtered$STATNAME[i]
  station_id <- LAKEMAP_filtered$STATIONID[i]

  # Sanitize file name (replace spaces and special chars)
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", paste0(station, "_", lake))

  message("Rendering report for: ", lake, " (", station, ")")

  # Render report
  render(
    input = template_path,
    output_format = "word_document",
    output_file = paste0(safe_name, "_Report.docx"),
    output_dir = output_dir,
    params = list(
      lake = lake,
      station = station,
      station_id = station_id
    ),
    envir = new.env() # isolate each run
  )
}
