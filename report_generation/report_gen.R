report_gen <- function() {
  library(rmarkdown)
  library(dplyr)

  # Load env file if present
  if (file.exists(".env")) {
    if (!requireNamespace("dotenv", quietly = TRUE)) {
      install.packages("dotenv")
    }
    library(dotenv)
    load_dot_env(".env")
  }

  # Define paths from .env
  input_path <- Sys.getenv("INPUT_PATH")
  report_path <- Sys.getenv("REPORT_PATH")
  template_path <- Sys.getenv("TEMPLATE_PATH")

  # Verify paths exist
  if (!dir.exists(input_path)) {
    stop("Input path does not exist: ", input_path)
  }
  if (!dir.exists(template_path)) {
    stop("Template path does not exist: ", template_path)
  }

  # Load LAKEMAP
  LAKEMAP <- read.csv(
    file = file.path(input_path, "LAKEMAP.csv"),
    stringsAsFactors = FALSE
  )

  # Filter for DEEP stations and keep unique lake–station combos
  LAKEMAP_filtered <- LAKEMAP |>
    filter(grepl("DEEP", STATNAME, ignore.case = TRUE)) |>
    distinct(RELLAKE, STATNAME, STATIONID, TOWN, .keep_all = TRUE) |>
    arrange(STATIONID)

  # Define output directory
  output_dir <- report_path
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  } else {
    message("Output directory already exists: ", output_dir)
  }

  # Define template path
  template_file <- file.path(template_path, "report_template.Rmd")
  if (!file.exists(template_file)) {
    stop("Template not found at: ", template_file)
  }

  # Loop through each unique DEEP station
  for (i in seq_len(nrow(LAKEMAP_filtered))) {
    lake <- LAKEMAP_filtered$RELLAKE[i]
    station <- LAKEMAP_filtered$STATNAME[i]
    station_id <- LAKEMAP_filtered$STATIONID[i]
    town <- LAKEMAP_filtered$TOWN[i]
    lake_full <- LAKEMAP_filtered$LAKE_FULL[i]

    # Clean file name
    safe_name <- gsub(
      "[^A-Za-z0-9_-]",
      "_",
      paste0(station, "_", town)
    )

    message("Rendering report for station: ", station, " (", town, ")")

    # Render report
    rmarkdown::render(
      input = template_file,
      output_format = "word_document",
      output_file = paste0(safe_name, "_Report.docx"),
      output_dir = output_dir,
      params = list(
        lake = lake,
        station = station,
        station_id = station_id,
        town = town,
        lake_full = lake_full
      ),
      envir = new.env() # isolate each run
    )
  }
}
