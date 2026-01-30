make_CYA_table <- function(data_long, table_path, input_path) {
  # create output directory if needed
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # read lake mapping file
  LAKEMAP <- read.csv(file.path(input_path, "LAKEMAP.csv"))

  # base CYA table
  CYA_base <- data_long |>
    select(
      RELLAKE,
      STATNAME,
      STATIONID,
      TOWN,
      DEPTHZONE,
      STARTDATE,
      WSHEDPARMNAME,
      NUMRESULT,
      ANALYTICALMETHOD
    ) |>
    mutate(
      STARTDATE = as.Date(STARTDATE, format = "%d-%b-%y"),
      YEAR = year(STARTDATE),
      param_depth = case_when(
        WSHEDPARMNAME %in%
          c(
            "ALKALINITY, TOTAL",
            "ALKALINITY, CARBONATE AS CACO3",
            "GRAN ACID NEUTRALIZING CAPACITY"
          ) ~ "Alk. (mg/L)",
        WSHEDPARMNAME ==
          "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" ~ "Chlor-a (μg/L)",
        WSHEDPARMNAME == "CHLORIDE" ~ "Chloride (mg/L)",
        WSHEDPARMNAME == "APPARENT COLOR" ~ "Color (pcu)",
        WSHEDPARMNAME == "SPECIFIC CONDUCTANCE" ~ "Cond. (μS/cm)",
        WSHEDPARMNAME == "ESCHERICHIA COLI" ~ "E. coli (mpn/100 mL)",
        WSHEDPARMNAME == "PHOSPHORUS AS P" ~ "Total P (μg/L)",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" &
          ANALYTICALMETHOD == "SECCHI" ~ "Trans. NVS (m)",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" &
          ANALYTICALMETHOD == "SECCHI-SCOPE" ~ "Trans. VS (m)",
        WSHEDPARMNAME == "TURBIDITY" ~ "Turb. (ntu)",
        WSHEDPARMNAME == "PH" ~ "pH",
        TRUE ~ NA_character_
      ),
      STATNAME = case_when(
        str_detect(STATNAME, "DEEP SPOT") ~ if_else(
          toupper(DEPTHZONE) == "COMPOSITE",
          "Epilimnion",
          str_to_title(DEPTHZONE)
        ),
        str_detect(STATNAME, "-") ~ str_trim(str_to_title(str_split_fixed(
          STATNAME,
          "-",
          2
        )[, 2])),
        TRUE ~ STATNAME
      )
    ) |>
    filter(!is.na(param_depth))

  # aggregate 2025
  CYA_2025 <- CYA_base |>
    filter(YEAR == 2025) |>
    group_by(RELLAKE, STATNAME, STATIONID, TOWN, param_depth) |>
    summarise(avg_result = mean(NUMRESULT, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = param_depth, values_from = avg_result) |>
    arrange(
      RELLAKE,
      factor(STATNAME, levels = c("Epilimnion", "Metalimnion", "Hypolimnion"))
    ) |>
    select(where(~ !all(is.na(.))))

  # round numeric columns
  numeric_cols <- c(
    "Alk. (mg/L)",
    "Chlor-a (μg/L)",
    "Chloride (mg/L)",
    "Color (pcu)",
    "Cond. (μS/cm)",
    "Total P (μg/L)",
    "Trans. NVS (m)",
    "Trans. VS (m)",
    "pH",
    "E. coli (mpn/100 mL)"
  )
  CYA_2025 <- CYA_2025 |>
    mutate(across(
      any_of(numeric_cols),
      ~ if (is.numeric(.)) round(., 2) else .
    ))

  # join with lake map to update RELLAKE names
  CYA_updated <- CYA_2025 |>
    left_join(LAKEMAP |> select(STATIONID, lake = RELLAKE), by = "STATIONID") |>
    mutate(RELLAKE = ifelse(!is.na(lake), lake, RELLAKE)) |>
    select(-lake)

  # ── HANDLE MULTI-TOWN LAKES ─────────────────────────────
  # create prefix (first 6 chars) for grouping
  CYA_updated <- CYA_updated |>
    mutate(STN_PREFIX = str_sub(STATIONID, 1, 6))

  CYA_updated <- CYA_updated |>
    mutate(
      TOWN = ifelse(
        RELLAKE == "LONG POND" & TOWN %in% c("PELHAM", "DRACUT"),
        "PELHAM",
        TOWN
      )
    )

  # find dominant town per lake + prefix
  dominant_town <- CYA_updated |>
    group_by(RELLAKE, STN_PREFIX) |>
    count(TOWN, sort = TRUE) |>
    slice_max(n, n = 1) |>
    ungroup() |>
    select(RELLAKE, STN_PREFIX, DOM_TOWN = TOWN)

  # update TOWN to dominant town per prefix
  CYA_updated <- CYA_updated |>
    left_join(dominant_town, by = c("RELLAKE", "STN_PREFIX")) |>
    mutate(TOWN = DOM_TOWN) |>
    select(-STN_PREFIX, -DOM_TOWN)

  # ── EXPORT TABLES ───────────────────────────────────────
  lake_town_pairs <- CYA_updated |> distinct(RELLAKE, TOWN)

  for (i in seq_len(nrow(lake_town_pairs))) {
    lake <- lake_town_pairs$RELLAKE[i]
    town <- lake_town_pairs$TOWN[i]

    lake_data <- CYA_updated |> filter(RELLAKE == lake, TOWN == town)
    lake_data_out <- lake_data |> select(-RELLAKE, -STATIONID, -TOWN)

    # DROP any column that is entirely NA for this lake/town
    lake_data_out <- lake_data_out |> select(where(~ !all(is.na(.))))

    file_name <- paste0(
      "CYA_",
      gsub(" ", "_", lake),
      "_",
      gsub(" ", "_", town),
      ".csv"
    )
    write_csv(lake_data_out, file.path(table_path, file_name), na = "-")
  }

  return(CYA_update = CYA_updated)

  message(
    "CYA tables exported for ",
    nrow(lake_town_pairs),
    " lake/town combinations to: ",
    table_path
  )
}
