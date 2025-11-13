make_CYA_table <- function(CYA, LAKEMAP, table_path, input_path) {
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # Join with LAKEMAP instead of lookup_table
  CYA_updated <- CYA |>
    left_join(
      LAKEMAP |> select(STATIONID, lake = RELLAKE),
      by = "STATIONID"
    ) |>
    mutate(RELLAKE = ifelse(!is.na(lake), lake, RELLAKE)) |>
    select(-lake)

  # Export CSVs for each lake found in LAKEMAP
  unique_lakes <- unique(LAKEMAP$RELLAKE)
  for (lake in unique_lakes) {
    lake_data <- CYA_updated |> filter(RELLAKE == lake)

    # Remove RELLAKE, STATIONID, and TOWN before export
    lake_data_out <- lake_data |> select(-RELLAKE, -STATIONID, -TOWN)

    file_name <- paste0(gsub(" ", "_", lake), "_CYA.csv")
    write_csv(lake_data_out, file.path(table_path, file_name))
  }

  message(
    "CYA tables exported for ",
    length(unique_lakes),
    " lakes to: ",
    table_path
  )
}
