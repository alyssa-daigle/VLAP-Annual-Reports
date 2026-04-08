report_gen <- function() {
  # Load .env if present
  if (file.exists(".env")) {
    if (!requireNamespace("dotenv", quietly = TRUE)) {
      install.packages("dotenv")
    }
    library(dotenv)
    load_dot_env(".env")
  }

  # Load required libraries
  library(dplyr)
  library(rmarkdown)
  library(officer)
  library(stringr)

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
  if (!dir.exists(report_path)) {
    dir.create(report_path, recursive = TRUE)
  }
  LAKEMAP <- read.csv(paste0(
    input_path,
    "LAKEMAP.csv"
  ))

  lake_reports <- CYA_updated |>
    distinct(RELLAKE, TOWN)

  for (i in seq_len(nrow(lake_reports))) {
    lake <- lake_reports$RELLAKE[i]
    town <- lake_reports$TOWN[i]

    # Collect stations
    stations <- CYA_updated |>
      filter(RELLAKE == lake, TOWN == town) |>
      pull(STATIONID) |>
      unique()

    # --- GET PUB VALUE ---
    pub_val <- LAKEMAP |>
      filter(RELLAKE == lake, TOWN == town) |>
      pull(PUB) |>
      unique()

    # Safeguards
    if (length(pub_val) == 0 || is.na(pub_val)) {
      pub_val <- ""
    } else {
      pub_val <- as.character(pub_val[1])
    }

    # Output file path
    output_file <- file.path(
      report_path,
      paste0(
        "2025VLAPReport_",
        gsub(" ", "_", lake),
        "_",
        gsub(" ", "_", town),
        ".docx"
      )
    )

    # --- RENDER REPORT ---
    rmarkdown::render(
      input = file.path(template_path, "report_template.Rmd"),
      output_file = output_file,
      params = list(
        lake = lake,
        town = town,
        stations = stations,
        table_path = table_path,
        pub = pub_val
      ),
      envir = new.env()
    )

    # --- POST-PROCESS DOCUMENT ---
    doc <- officer::read_docx(output_file)

    # Replace header placeholder (<<pub>>)
    doc <- officer::headers_replace_all_text(
      doc,
      old_value = "<<pub>>",
      new_value = pub_val,
      only_at_cursor = FALSE
    )

    # Build tags (keywords)
    tags <- c(
      "vlap",
      "report",
      "2025",
      "Watershed Management",
      "vlapreport",
      lake,
      town
    )

    # Set document properties (title + keywords)
    doc <- officer::set_doc_properties(
      doc,
      title = paste(
        str_to_title(lake),
        str_to_title(town),
        "2025 VLAP Report"
      ),
      subject = paste(tags, collapse = ", "), # <-- THIS shows as Tags in Word
      keywords = paste(tags, collapse = ", ") # optional (kept for completeness)
    )

    # Save final document
    print(doc, target = output_file)
  }

  message("All reports generated in: ", report_path)
}
