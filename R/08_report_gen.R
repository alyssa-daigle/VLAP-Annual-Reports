report_gen <- function() {
  # Load .env if present
  if (file.exists(".env")) {
    if (!requireNamespace("dotenv", quietly = TRUE)) {
      install.packages("dotenv")
    }
    library(dotenv)
    load_dot_env(".env")
  }

  # Paths
  input_path <- Sys.getenv("INPUT_PATH")
  report_path <- Sys.getenv("REPORT_PATH")
  template_path <- Sys.getenv("TEMPLATE_PATH")
  table_path <- Sys.getenv("TABLE_PATH")

  if (!dir.exists(input_path)) {
    stop("Input path does not exist: ", input_path)
  }
  if (!dir.exists(template_path)) {
    stop("Template path does not exist: ", template_path)
  }

  # Load LAKEMAP
  LAKEMAP <- read.csv(
    file.path(input_path, "LAKEMAP.csv"),
    stringsAsFactors = FALSE
  )

  # --- Filter stations ---
  LAKEMAP_filtered <- LAKEMAP |>
    distinct(WATERBODYNAME, TOWN, STATIONID, .keep_all = TRUE) |>
    arrange(WATERBODYNAME, TOWN, STATIONID) |>
    filter(
      grepl("^SUNAPEE LAKE", WATERBODYNAME, ignore.case = TRUE) |
        grepl("DEEP", STATNAME, ignore.case = TRUE)
    ) |>
    arrange(WATERBODYNAME, TOWN, STATIONID)

  # Create output directory if needed
  if (!dir.exists(report_path)) {
    dir.create(report_path, recursive = TRUE)
  }

  template_file <- file.path(template_path, "report_template.Rmd")
  if (!file.exists(template_file)) {
    stop("Template not found at: ", template_file)
  }

  # --- Sunapee NEARSHORE + TRIB combined report ---
  sunapee_stations <- LAKEMAP_filtered |>
    filter(
      grepl("sunapee", WATERBODYNAME, ignore.case = TRUE) &
        grepl("NEARSHORE|TRIBS", STATNAME, ignore.case = TRUE)
    )

  if (nrow(sunapee_stations) > 0) {
    lake <- "SUNAPEE"
    station <- "NEARSHORE + TRIB"
    # pass vector of unique station IDs
    station_id <- sunapee_stations$STATIONID
    town <- paste(unique(sunapee_stations$TOWN), collapse = ", ")
    safe_name <- gsub("[^A-Za-z0-9_-]", "_", paste0(station, "_", town))

    message(
      "Rendering combined report for SUNAPEE NEARSHORE + TRIB (",
      length(station_id),
      " stations)"
    )

    rmarkdown::render(
      input = template_file,
      output_format = "word_document",
      output_file = paste0(safe_name, "_Report.docx"),
      output_dir = report_path,
      params = list(
        lake = lake,
        station = station,
        station_id = station_id, # vector of IDs
        town = town,
        table_path = table_path
      ),
      envir = new.env()
    )
  }

  # --- Individual reports for other DEEP stations ---
  other_stations <- LAKEMAP_filtered |>
    filter(
      !(grepl("sunapee", WATERBODYNAME, ignore.case = TRUE) &
        grepl("NEARSHORE|TRIBS", STATNAME, ignore.case = TRUE))
    )

  for (i in seq_len(nrow(other_stations))) {
    lake <- other_stations$WATERBODYNAME[i]
    station <- other_stations$STATNAME[i]
    station_id <- other_stations$STATIONID[i]
    town <- other_stations$TOWN[i]

    safe_name <- gsub("[^A-Za-z0-9_-]", "_", paste0(station, "_", town))
    message("Rendering report for: ", lake, " - ", station, " (", town, ")")

    rmarkdown::render(
      input = template_file,
      output_format = "word_document",
      output_file = paste0(safe_name, "_Report.docx"),
      output_dir = report_path,
      params = list(
        lake = lake,
        station = station,
        station_id = station_id,
        town = town,
        table_path = table_path
      ),
      envir = new.env()
    )
  }

  message("All reports generated!")
}
