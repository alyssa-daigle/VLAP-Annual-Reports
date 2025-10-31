make_CYA_table <- function(CYA, table_path) {
  # create output directory if missing
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  lakes_2025 <- CYA |>
    distinct(RELLAKE) |>
    arrange(RELLAKE) |>
    pull(RELLAKE)

  write_csv(
    tibble(Lake = lakes_2025),
    file.path(table_path, "Lakes_with_2025_data.csv")
  )

  # get unique lakes
  lakes <- unique(CYA$RELLAKE)

  # loop through each lake and save CSV
  for (lake in lakes) {
    # subset data for this lake
    lake_data <- CYA |> filter(RELLAKE == lake)

    # remove RELLAKE column if not needed
    lake_data_out <- lake_data |> select(-RELLAKE)

    # build file name (replace spaces with underscores)
    file_name <- paste0(gsub(" ", "_", lake), "_CYA.csv")

    # write CSV
    write_csv(lake_data_out, file.path(table_path, file_name))
  }

  message("CYA tables exported for ", length(lakes), " lakes to: ", table_path)
}
