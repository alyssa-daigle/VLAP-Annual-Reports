data_reformat <- function(BTC_full, REG_long, CYA_full) {
  # BTC (best Trophic Class) processing
  # set factors where 1 = "best" and 3 = "worst"
  trophic_map <- c(
    "OLIGOTROPHIC" = 1,
    "MESOTROPHIC" = 2,
    "EUTROPHIC" = 3
  )

  # reformat so only the "best" trophic class is retained per lake
  BTC <- BTC_full |>
    select(RELLAKE, BEST_TROPHIC_CLASS) |>
    rename(lake = RELLAKE, BTC = BEST_TROPHIC_CLASS) |>
    mutate(BTC_num = trophic_map[BTC]) |>
    group_by(lake) |>
    summarise(
      BTC = BTC[which.min(BTC_num)],
      .groups = "drop"
    )

  #REG dataset
  # REG (regression dataset) processing ----
  DETECTION_LIMIT <- 0.005 # mg/L for TP
  HALF_DL <- DETECTION_LIMIT / 2 # 0.0025 mg/L

  REG <- REG_long |>
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
    mutate(
      # Handle non-detects for Total Phosphorus
      NUMRESULT = case_when(
        WSHEDPARMNAME == "PHOSPHORUS AS P" &
          str_detect(toupper(TEXTRESULT), "ND") ~ HALF_DL,
        TRUE ~ NUMRESULT
      ),
      param_depth = case_when(
        WSHEDPARMNAME == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" &
          DEPTHZONE == "COMPOSITE" ~ "CHL_comp",
        WSHEDPARMNAME == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" &
          DEPTHZONE == "EPILIMNION" ~ "CHL_epi",
        WSHEDPARMNAME == "SPECIFIC CONDUCTANCE" &
          DEPTHZONE == "EPILIMNION" ~ "SPCD_epi",
        WSHEDPARMNAME == "PH" & DEPTHZONE == "EPILIMNION" ~ "PH_epi",
        WSHEDPARMNAME == "PHOSPHORUS AS P" &
          DEPTHZONE == "EPILIMNION" ~ "TP_epi",
        WSHEDPARMNAME == "PHOSPHORUS AS P" &
          DEPTHZONE == "HYPOLIMNION" ~ "TP_hypo",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" ~ "SECCHI",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(param_depth)) |>
    pivot_wider(
      id_cols = c(RELLAKE, TOWN, STATIONID, STATNAME, STARTDATE),
      names_from = param_depth,
      values_from = NUMRESULT,
      values_fn = \(x) mean(x[x >= 0], na.rm = TRUE)
    ) |>
    rename(
      lake = RELLAKE,
      town = TOWN,
      stationid = STATIONID,
      stationname = STATNAME,
      date = STARTDATE
    ) |>
    mutate(Year = year(date)) |>
    group_by(stationid, Year) |>
    summarise(
      lake = first(lake),
      stationname = first(stationname),
      across(
        c(CHL_comp, CHL_epi, SPCD_epi, PH_epi, TP_epi, TP_hypo, SECCHI),
        mean,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(
      TP_epi = TP_epi * 1000, # convert mg/L → µg/L
      TP_hypo = TP_hypo * 1000
    )

  # CYA (Current Year Averages) processing
  CYA <- CYA_full |>
    select(
      RELLAKE,
      STATNAME,
      STATIONID,
      DEPTHZONE,
      PYEAR,
      WSHEDPARMNAME,
      NUMRESULT,
      ANALYTICALMETHOD
    ) |>
    filter(PYEAR == 2025) |>
    mutate(
      param_depth = case_when(
        WSHEDPARMNAME == "ALKALINITY, CARBONATE AS CACO3" ~ "Alk. (mg/L)",
        WSHEDPARMNAME == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" ~
          "Chlor-a (μg/L)",
        WSHEDPARMNAME == "CHLORIDE" ~ "Chloride (mg/L)",
        WSHEDPARMNAME == "APPARENT COLOR" ~ "Color (pcu)",
        WSHEDPARMNAME == "SPECIFIC CONDUCTANCE" ~ "Cond. (μS/cm)",
        WSHEDPARMNAME == "ESCHERICHIA COLI" ~ "E. coli (mpn/100 mL)",
        WSHEDPARMNAME == "PHOSPHORUS AS P" ~ "Total P (μg/L)",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" &
          ANALYTICALMETHOD == "SECCHI" ~
          "Trans. NVS (m)",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" &
          ANALYTICALMETHOD == "SECCHI-SCOPE" ~
          "Trans. VS (m)",
        WSHEDPARMNAME == "TURBIDITY" ~ "Turb. (ntu)",
        WSHEDPARMNAME == "PH" ~ "pH",
        TRUE ~ NA_character_
      ),
      STATNAME = case_when(
        # Deep spot logic
        str_detect(STATNAME, "DEEP SPOT") ~
          case_when(
            toupper(DEPTHZONE) == "COMPOSITE" ~ "Epilimnion",
            TRUE ~ str_to_title(DEPTHZONE)
          ),
        # Regular stations: remove lake prefix
        str_detect(STATNAME, "-") ~
          str_trim(str_to_title(str_split_fixed(STATNAME, "-", 2)[, 2])),
        TRUE ~ STATNAME
      )
    ) |>
    filter(!is.na(param_depth)) |>
    group_by(RELLAKE, STATNAME, param_depth) |>
    summarise(avg_result = mean(NUMRESULT, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(
      names_from = param_depth,
      values_from = avg_result
    ) |>
    arrange(
      RELLAKE,
      factor(STATNAME, levels = c("Epilimnion", "Metalimnion", "Hypolimnion"))
    ) |>
    mutate(`Total P (μg/L)` = `Total P (μg/L)` * 1000)

  LAKEMAP <- CYA_full |>
    select(RELLAKE, STATNAME, STATIONID, TOWN) |>
    distinct()

  write.csv(
    LAKEMAP,
    file = file.path(input_path, "LAKEMAP.csv"),
    row.names = FALSE
  )

  # return list of tidy dataframes
  list(BTC = BTC, REG = REG, CYA = CYA, LAKEMAP = LAKEMAP)
}
