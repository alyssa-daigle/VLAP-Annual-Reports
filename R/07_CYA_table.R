make_CYA_table <- function(data_long, table_path, input_path) {
  # create output directory if it does not exist
  if (!dir.exists(table_path)) {
    dir.create(table_path, recursive = TRUE)
  }

  # read lake mapping file
  LAKEMAP <- read.csv(paste0(input_path, "LAKEMAP.csv"))

  # select relevant columns and extract year from startdate
  CYA_base <- data_long |>
    select(
      WATERBODYNAME,
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
      # convert startdate to date format
      STARTDATE = as.Date(STARTDATE, format = "%d-%b-%y"),
      # extract year from startdate
      YEAR = year(as.Date(STARTDATE))
    ) |>
    # map parameter names and standardize statname
    mutate(
      param_depth = case_when(
        WSHEDPARMNAME == "ALKALINITY, TOTAL" ~ "Alk. (mg/L)",
        WSHEDPARMNAME == "ALKALINITY, CARBONATE AS CACO3" ~ "Alk. (mg/L)",
        WSHEDPARMNAME == "GRAN ACID NEUTRALIZING CAPACITY" ~ "Alk. (mg/L)",
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
        str_detect(STATNAME, "DEEP SPOT") ~ case_when(
          toupper(DEPTHZONE) == "COMPOSITE" ~ "Epilimnion",
          TRUE ~ str_to_title(DEPTHZONE)
        ),
        str_detect(STATNAME, "-") ~ str_trim(str_to_title(str_split_fixed(
          STATNAME,
          "-",
          2
        )[, 2])),
        TRUE ~ STATNAME
      )
    ) |>
    # remove rows without a mapped parameter
    filter(!is.na(param_depth))

  # filter data for 2025 and compute average results by lake, station, and parameter
  CYA_2025 <- CYA_base |>
    filter(YEAR == 2025) |>
    group_by(WATERBODYNAME, STATNAME, STATIONID, TOWN, param_depth) |>
    summarise(avg_result = mean(NUMRESULT, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(
      names_from = param_depth,
      values_from = avg_result
    ) |>
    # order by lake and standard depth labels
    arrange(
      WATERBODYNAME,
      factor(STATNAME, levels = c("Epilimnion", "Metalimnion", "Hypolimnion"))
    ) |>
    # round numeric columns for presentation
    mutate(
      `Alk. (mg/L)` = round(`Alk. (mg/L)`, 1),
      `Chlor-a (μg/L)` = round(`Chlor-a (μg/L)`, 2),
      `Chloride (mg/L)` = round(`Chloride (mg/L)`, 0),
      `Color (pcu)` = round(`Color (pcu)`, 0),
      `Cond. (μS/cm)` = round(`Cond. (μS/cm)`, 1),
      `E. coli (mpn/100 mL)` = round(`E. coli (mpn/100 mL)`, 0),
      `Total P (μg/L)` = round(`Total P (μg/L)`, 0),
      `Trans. NVS (m)` = round(`Trans. NVS (m)`, 2),
      `Trans. VS (m)` = round(`Trans. VS (m)`, 2),
      `Total P (μg/L)` = case_when(
        is.na(`Total P (μg/L)`) ~ NA_character_,
        `Total P (μg/L)` < 5 ~ "<5",
        TRUE ~ as.character(round(`Total P (μg/L)`, 0))
      ),
      pH = round(pH, 2)
    )

  # join with lake map to update lake names if available
  CYA_updated <- CYA_2025 |>
    left_join(
      LAKEMAP |> select(STATIONID, lake = WATERBODYNAME),
      by = "STATIONID"
    ) |>
    mutate(WATERBODYNAME = ifelse(!is.na(lake), lake, WATERBODYNAME)) |>
    select(-lake)

  # get all unique lake and town combinations
  lake_town_pairs <- CYA_updated |> distinct(WATERBODYNAME, TOWN)

  # export tables for each lake and town combination
  for (i in seq_len(nrow(lake_town_pairs))) {
    lake <- lake_town_pairs$WATERBODYNAME[i]
    town <- lake_town_pairs$TOWN[i]

    lake_data <- CYA_updated |>
      filter(WATERBODYNAME == lake, TOWN == town)

    # remove lake and station identifiers before export
    lake_data_out <- lake_data |> select(-WATERBODYNAME, -STATIONID, -TOWN)

    # clean file name
    lake_clean <- gsub(" ", "_", lake)
    town_clean <- gsub(" ", "_", town)
    file_name <- paste0("CYA_", lake_clean, "_", town_clean, ".csv")

    # write csv with na as dash
    write_csv(lake_data_out, file.path(table_path, file_name), na = "-")
  }

  # message to indicate completion
  message(
    "CYA tables exported for ",
    nrow(lake_town_pairs),
    " lake/town combinations to: ",
    table_path
  )
}
