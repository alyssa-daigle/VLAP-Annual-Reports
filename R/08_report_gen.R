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
  if (!dir.exists(report_path)) {
    dir.create(report_path, recursive = TRUE)
  }

  lake_reports <- CYA_updated |>
    distinct(RELLAKE, TOWN)

  for (i in seq_len(nrow(lake_reports))) {
    lake <- lake_reports$RELLAKE[i]
    town <- lake_reports$TOWN[i]

    # collect all stations for this lake/town
    stations <- CYA_updated |>
      filter(RELLAKE == lake, TOWN == town) |>
      pull(STATIONID) |>
      unique()

    # Render report once for this lake/town combo
    rmarkdown::render(
      input = file.path(template_path, "report_template.Rmd"),
      output_file = file.path(
        report_path,
        paste0(
          "2025VLAPReport_", #UPDATE YEAR EVERY YEAR
          gsub(" ", "_", lake),
          "_",
          gsub(" ", "_", town),
          ".docx"
        )
      ),
      params = list(
        lake = lake,
        town = town,
        stations = stations,
        table_path = table_path
      ),
      envir = new.env()
    )
  }

  message("All reports generated in: ", report_path)
}
