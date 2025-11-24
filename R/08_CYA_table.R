make_CYA_table <- function(CYA, LAKEMAP, table_path, input_path) {
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # Join with LAKEMAP to get updated lake names
  CYA_updated <- CYA |>
    left_join(
      LAKEMAP |> select(STATIONID, lake = RELLAKE),
      by = "STATIONID"
    ) |>
    mutate(RELLAKE = ifelse(!is.na(lake), lake, RELLAKE)) |>
    select(-lake)

  # Export tables grouped by BOTH lake and town
  lake_town_pairs <- CYA_updated |> distinct(RELLAKE, TOWN)

  for (i in seq_len(nrow(lake_town_pairs))) {
    lake <- lake_town_pairs$RELLAKE[i]
    town <- lake_town_pairs$TOWN[i]

    lake_data <- CYA_updated |>
      filter(RELLAKE == lake, TOWN == town)

    # Remove RELLAKE, STATIONID, and TOWN before export
    lake_data_out <- lake_data |> select(-RELLAKE, -STATIONID, -TOWN)

    # Clean file name: Lake_Town_CYA.csv
    lake_clean <- gsub(" ", "_", lake)
    town_clean <- gsub(" ", "_", town)
    file_name <- paste0(lake_clean, "_", town_clean, "_CYA.csv")

    write_csv(lake_data_out, file.path(table_path, file_name))
  }

  message(
    "CYA tables exported for ",
    nrow(lake_town_pairs),
    " lake/town combinations to: ",
    table_path
  )
}
