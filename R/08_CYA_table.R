make_CYA_table <- function(CYA, table_path, input_path) {
  library(dplyr)
  library(readr)
  library(readxl)

  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  lookup_file <- file.path(input_path, "lookup.xlsx")
  if (!file.exists(lookup_file)) {
    stop("Lookup table not found at: ", lookup_file)
  }
  lookup_table <- read_excel(lookup_file) |>
    distinct(WQDStationID, .keep_all = TRUE) # ensure one lake per station

  CYA_updated <- CYA |>
    left_join(
      lookup_table |> select(WQDStationID, lake),
      by = c("STATIONID" = "WQDStationID")
    ) |>
    mutate(RELLAKE = ifelse(!is.na(lake), lake, RELLAKE)) |>
    select(-lake)

  # export CSVs
  unique_lakes <- unique(CYA_updated$RELLAKE)
  for (lake in unique_lakes) {
    lake_data <- CYA_updated |> filter(RELLAKE == lake)
    file_name <- paste0(gsub(" ", "_", lake), "_CYA.csv")
    write_csv(lake_data, file.path(table_path, file_name))
  }

  message(
    "CYA tables exported for ",
    length(unique_lakes),
    " lakes to: ",
    table_path
  )
}
