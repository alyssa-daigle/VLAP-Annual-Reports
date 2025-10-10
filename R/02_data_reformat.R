data_reformat <- function(BTC_full, REG_long) {
  # --- BTC processing ---
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

  # --- REG processing ---
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
      NUMRESULT
    ) |>
    mutate(
      param_depth = case_when(
        WSHEDPARMNAME == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" &
          DEPTHZONE == "COMPOSITE" ~
          "CHL_comp",
        WSHEDPARMNAME == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" &
          DEPTHZONE == "EPILIMNION" ~
          "CHL_epi",
        WSHEDPARMNAME == "SPECIFIC CONDUCTANCE" & DEPTHZONE == "EPILIMNION" ~
          "SPCD_epi",
        WSHEDPARMNAME == "PH" & DEPTHZONE == "EPILIMNION" ~ "PH_epi",
        WSHEDPARMNAME == "PHOSPHORUS AS P" & DEPTHZONE == "EPILIMNION" ~
          "TP_epi",
        WSHEDPARMNAME == "PHOSPHORUS AS P" & DEPTHZONE == "HYPOLIMNION" ~
          "TP_hypo",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" ~ "SECCHI",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(param_depth)) |>
    pivot_wider(
      id_cols = c(RELLAKE, TOWN, STATIONID, STATNAME, STARTDATE),
      names_from = param_depth,
      values_from = NUMRESULT,
      values_fn = mean,
      values_fill = NA
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
      TP_epi = TP_epi * 1000, # convert to µg/L
      TP_hypo = TP_hypo * 1000
    )

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
      # ---- Clean parameter names ----
      param_depth = case_when(
        WSHEDPARMNAME == "ALKALINITY, CARBONATE AS CACO3" ~ "Alk. (mg/L)",
        WSHEDPARMNAME == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" ~
          "Chlor-a (ug/L)",
        WSHEDPARMNAME == "CHLORIDE" ~ "Chloride (mg/L)",
        WSHEDPARMNAME == "APPARENT COLOR" ~ "Color (pcu)",
        WSHEDPARMNAME == "SPECIFIC CONDUCTANCE" ~ "Cond. (us/cm)",
        WSHEDPARMNAME == "ESCHERICHIA COLI" ~ "E. coli (mpn/100 mL)",
        WSHEDPARMNAME == "PHOSPHORUS AS P" ~ "Total P (ug/L)",
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
      # ---- Clean station names ----
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
    )

  list(BTC = BTC, REG = REG, CYA = CYA)
}
