report_gen <- function() {
  # Load .env if present (optional legacy support)
  if (file.exists(".env")) {
    if (!requireNamespace("dotenv", quietly = TRUE)) {
      install.packages("dotenv")
    }
    library(dotenv)
    load_dot_env(".env")
  }

  # ---- sanity checks ----
  if (!dir.exists(INPUT_PATH)) {
    stop("Input path does not exist: ", INPUT_PATH)
  }
  if (!dir.exists(TEMPLATE_PATH)) {
    stop("Template path does not exist: ", TEMPLATE_PATH)
  }
  if (!dir.exists(REPORT_PATH)) {
    dir.create(REPORT_PATH, recursive = TRUE)
  }

  # ---- load lake map (YEAR-agnostic static lookup file) ----
  LAKEMAP <- read.csv(file.path(INPUT_PATH, "LAKEMAP.csv"))

  lake_reports <- CYA_updated |>
    distinct(RELLAKE, TOWN)

  for (i in seq_len(nrow(lake_reports))) {
    lake <- lake_reports$RELLAKE[i]
    town <- lake_reports$TOWN[i]

    # ---- stations ----
    stations <- CYA_updated |>
      filter(RELLAKE == lake, TOWN == town) |>
      pull(STATIONID) |>
      unique()

    # ---- PUB lookup ----
    pub_val <- LAKEMAP |>
      filter(RELLAKE == lake, TOWN == town) |>
      pull(PUB) |>
      unique()

    pub_val <- if (length(pub_val) == 0 || is.na(pub_val)) {
      ""
    } else {
      as.character(pub_val[1])
    }

    # ---- output file ----
    output_file <- file.path(
      REPORT_PATH,
      paste0(
        YEAR,
        "VLAPReport_",
        gsub(" ", "_", lake),
        "_",
        gsub(" ", "_", town),
        ".docx"
      )
    )

    # ---- render Rmd ----
    rmarkdown::render(
      input = file.path(TEMPLATE_PATH, "report_template.Rmd"),
      output_file = output_file,
      params = list(
        lake = lake,
        town = town,
        stations = stations,
        TABLE_PATH = TABLE_PATH,
        pub = pub_val,
        year = YEAR
      ),
      envir = new.env()
    )

    # ---- post-process Word doc ----
    doc <- officer::read_docx(output_file)

    doc <- officer::headers_replace_all_text(
      doc,
      old_value = "<<pub>>",
      new_value = pub_val,
      only_at_cursor = FALSE
    )

    # ---- tags ----
    tags <- c(
      "vlap",
      "report",
      YEAR,
      "Watershed Management",
      "vlapreport",
      lake,
      town
    )

    doc <- officer::set_doc_properties(
      doc,
      title = paste(
        str_to_title(lake),
        str_to_title(town),
        YEAR,
        "VLAP Report"
      ),
      subject = paste(tags, collapse = ", "),
      keywords = paste(tags, collapse = ", ")
    )

    print(doc, target = output_file)
  }

  message("All reports generated in: ", REPORT_PATH)
}
