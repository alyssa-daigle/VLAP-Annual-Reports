data_reformat <- function(input_path) {
  BTC_file <- file.path(input_path, "BTC_full.csv")
  REG_file <- file.path(input_path, "REG_long.csv")
  CYA_file <- file.path(input_path, "CYA_full.csv")

  BTC_full <- read_csv(BTC_file, show_col_types = FALSE)
  REG_long <- read_csv(REG_file, show_col_types = FALSE)
  CYA_full <- read_csv(CYA_file, show_col_types = FALSE)

  # -----------------------------
  # BTC (Best Trophic Class)
  # -----------------------------
  trophic_map <- c(
    "OLIGOTROPHIC" = 1,
    "MESOTROPHIC" = 2,
    "EUTROPHIC" = 3
  )

  BTC <- BTC_full |>
    select(RELLAKE, BEST_TROPHIC_CLASS) |>
    rename(lake = RELLAKE, BTC = BEST_TROPHIC_CLASS) |>
    mutate(BTC_num = trophic_map[BTC]) |>
    group_by(lake) |>
    summarise(
      BTC = BTC[which.min(BTC_num)],
      .groups = "drop"
    )

  # -----------------------------
  # Detection limit handling
  # -----------------------------
  DETECTION_LIMIT <- 0.005
  HALF_DL <- DETECTION_LIMIT / 2

  # -----------------------------
  # Station-specific start years
  # -----------------------------
  lake_start_years <- tibble::tibble(
    stationid = c(
      "ANGSDND",
      "COUKIND",
      "CRYMAND",
      "DORMAND",
      "EMERIND",
      "FLIHLSD",
      "FRAHSBD",
      "HOWDUBD",
      "JENNORD",
      "LONPELD",
      "PEMMERVLAPD",
      "PHISDND",
      "ROCFITD",
      "SAWGLMD",
      "TARPIED",
      "WARALSD",
      "WAUDAND",
      "WILPFDD"
    ),
    lake = c(
      "ANGLE POND",
      "COUNTRY POND",
      "CRYSTAL LAKE",
      "DORRS POND",
      "EMERSON POND",
      "FLINTS POND",
      "FRANKLIN PIERCE LAKE",
      "HOWE RESERVOIR",
      "JENNESS POND",
      "LONG POND",
      "PEMIGEWASSET LAKE",
      "PHILLIPS POND",
      "ROCKWOOD POND",
      "SAWYER LAKE",
      "LAKE TARLETON",
      "WARREN LAKE",
      "WAUKEENA LAKE",
      "WILD GOOSE POND"
    ),
    town = c(
      "SANDOWN",
      "KINGSTON",
      "MANCHESTER",
      "MANCHESTER",
      "RINDGE",
      "HOLLIS",
      "HILLSBOROUGH",
      "DUBLIN",
      "NORTHWOOD",
      "PELHAM",
      "MEREDITH",
      "SANDOWN",
      "FITZWILLIAM",
      "GILMANTON",
      "PIERMONT",
      "ALSTEAD",
      "DANBURY",
      "PITTSFIELD"
    ),
    start_year = c(
      2005,
      2018,
      1993,
      2000,
      2006,
      1991,
      2010,
      2011,
      1994,
      2006,
      2005,
      2007,
      2001,
      2009,
      2002,
      1999,
      2003,
      2013
    )
  )

  # -----------------------------
  # REG processing
  # -----------------------------
  REG1 <- REG_long |>
    filter(PROJID == "VLAP") |>
    select(
      RELLAKE,
      TOWN,
      STATIONID,
      STATNAME,
      STARTDATE,
      DEPTHZONE,
      WSHEDPARMNAME,
      NUMRESULT,
      TEXTRESULT
    ) |>
    filter(
      DEPTHZONE %in%
        c("EPILIMNION", "HYPOLIMNION", "COMPOSITE") |
        WSHEDPARMNAME %in%
          c(
            "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN",
            "SECCHI DISK TRANSPARENCY"
          )
    ) |>
    mutate(
      NUMRESULT = case_when(
        WSHEDPARMNAME == "PHOSPHORUS AS P" &
          str_detect(toupper(TEXTRESULT), "ND") ~ HALF_DL,
        TRUE ~ NUMRESULT
      ),
      NUMRESULT = case_when(
        WSHEDPARMNAME == "PHOSPHORUS AS P" ~ NUMRESULT * 1000,
        TRUE ~ NUMRESULT
      ),
      param_depth = case_when(
        WSHEDPARMNAME ==
          "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" ~ "CHL_comp",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" ~ "SECCHI",
        WSHEDPARMNAME == "SPECIFIC CONDUCTANCE" &
          DEPTHZONE == "EPILIMNION" ~ "SPCD_epi",
        WSHEDPARMNAME == "PH" & DEPTHZONE == "EPILIMNION" ~ "PH_epi",
        WSHEDPARMNAME == "PHOSPHORUS AS P" &
          DEPTHZONE == "EPILIMNION" ~ "TP_epi",
        WSHEDPARMNAME == "PHOSPHORUS AS P" &
          DEPTHZONE == "HYPOLIMNION" ~ "TP_hypo",
        TRUE ~ NA_character_
      ),
      stationid = case_when(
        STATIONID %in% c("ROCHLSVLAPD", "ROCHLSD") ~ "ROCHLSVLAPD",
        STATIONID %in% c("PEMMERVLAPD", "PEMMERD") ~ "PEMMERVLAPD",
        STATIONID %in% c("SPEGROVLAPD", "SPEGROD") ~ "SPEGROVLAPD",
        TRUE ~ STATIONID
      ),
      stationname = case_when(
        STATIONID %in% c("ROCHLSVLAPD", "ROCHLSD") ~
          "ROCKY POND-DEEP SPOT",
        STATIONID %in% c("PEMMERVLAPD", "PEMMERD") ~
          "PEMIGEWASSET LAKE-DEEP SPOT",
        STATIONID %in% c("SPEGROVLAPD", "SPEGROD") ~
          "SPECTACLE POND-DEEP SPOT",
        TRUE ~ str_trim(toupper(STATNAME))
      )
    )

  REG2 <- REG1 |>
    filter(!is.na(param_depth)) |>
    pivot_wider(
      id_cols = c(RELLAKE, TOWN, stationid, stationname, STARTDATE),
      names_from = param_depth,
      values_from = NUMRESULT,
      values_fn = \(x) mean(x, na.rm = TRUE)
    ) |>
    rename(lake = RELLAKE, town = TOWN, date = STARTDATE) |>
    mutate(Year = year(date))

  REG <- REG2 |>
    left_join(
      lake_start_years |> select(stationid, start_year),
      by = "stationid"
    ) |>
    filter(is.na(start_year) | Year >= start_year) |>
    group_by(lake, stationid, Year) |>
    summarise(
      stationname = first(stationname),
      across(
        c(CHL_comp, SECCHI, SPCD_epi, PH_epi, TP_epi, TP_hypo),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  # -----------------------------
  # CYA (Current Year Averages) processing
  # -----------------------------
  CYA_base <- CYA_full |>
    select(
      RELLAKE,
      STATNAME,
      STATIONID,
      TOWN,
      DEPTHZONE,
      PYEAR,
      WSHEDPARMNAME,
      NUMRESULT,
      ANALYTICALMETHOD
    ) |>

    # Map parameter names
    mutate(
      param_depth = case_when(
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

      # Standardize STATNAME depth labels
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

    filter(!is.na(param_depth))

  CYA_2025 <- CYA_base |>
    filter(PYEAR == 2025) |>
    group_by(RELLAKE, STATNAME, STATIONID, TOWN, param_depth) |>
    summarise(avg_result = mean(NUMRESULT, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(
      names_from = param_depth,
      values_from = avg_result
    ) |>

    # Convert TP nondetect (still NA) → "<5"
    mutate(
      `Total P (μg/L)` = case_when(
        is.na(`Total P (μg/L)`) ~ "<5", # all ND
        TRUE ~ as.character(`Total P (μg/L)` * 1000) # convert mg/L → µg/L
      )
    ) |>

    arrange(
      RELLAKE,
      factor(STATNAME, levels = c("Epilimnion", "Metalimnion", "Hypolimnion"))
    ) |>

    # Final rounding and formatting
    mutate(
      `Alk. (mg/L)` = round(`Alk. (mg/L)`, 1),
      `Chlor-a (μg/L)` = round(`Chlor-a (μg/L)`, 2),
      `Chloride (mg/L)` = round(`Chloride (mg/L)`, 0),
      `Color (pcu)` = round(`Color (pcu)`, 0),
      `E. coli (mpn/100 mL)` = round(`E. coli (mpn/100 mL)`, 0),
      `Trans. NVS (m)` = round(`Trans. NVS (m)`, 2),
      `Trans. VS (m)` = round(`Trans. VS (m)`, 2),
      `Turb. (ntu)` = round(`Turb. (ntu)`, 2),
      pH = round(pH, 2),

      # Final TP formatting
      `Total P (μg/L)` = if_else(
        `Total P (μg/L)` == "<5",
        "<5",
        as.character(round(as.numeric(`Total P (μg/L)`), 0))
      )
    )

  CYA_long <- CYA_base |>
    group_by(
      RELLAKE,
      STATIONID,
      STATNAME,
      TOWN,
      PYEAR,
      param_depth
    ) |>
    summarise(
      avg_result = mean(NUMRESULT, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(
      Year = PYEAR
    ) |>
    # apply lake + town specific start years
    left_join(
      lake_start_years,
      by = c(
        "RELLAKE" = "lake",
        "TOWN" = "town"
      )
    ) |>
    filter(is.na(start_year) | Year >= start_year) |>
    select(-start_year)

  # Get the list of station IDs that have 2025 CYA data
  cya_stations <- unique(CYA_2025$STATIONID)

  # Subset CYA_full to only include those stations, keeping original STATNAME
  LAKEMAP_partial <- CYA_full |>
    filter(STATIONID %in% cya_stations) |>
    select(RELLAKE, STATNAME, STATIONID, TOWN) |>
    distinct()

  lookup_file <- file.path(input_path, "lookup.xlsx")
  if (!file.exists(lookup_file)) {
    stop("Lookup table not found at: ", lookup_file)
  }

  lookup_table <- read_excel(lookup_file) |>
    distinct(WQDStationID, .keep_all = TRUE) # ensure one lake per station

  LAKEMAP <- LAKEMAP_partial |>
    left_join(
      lookup_table |> select(WQDStationID, lake),
      by = c("STATIONID" = "WQDStationID")
    ) |>
    mutate(RELLAKE = ifelse(!is.na(lake), lake, RELLAKE)) |>
    select(-lake)

  write.csv(
    LAKEMAP,
    file = file.path(input_path, "LAKEMAP.csv"),
    row.names = FALSE
  )

  # return list of tidy dataframes
  list(
    BTC = BTC,
    REG = REG,
    CYA_2025 = CYA_2025,
    CYA_long = CYA_long,
    LAKEMAP = LAKEMAP
  )
}
