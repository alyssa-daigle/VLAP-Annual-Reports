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

  list(BTC = BTC, REG = REG)
}
