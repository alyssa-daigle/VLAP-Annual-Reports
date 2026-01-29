data_reformat <- function(input_path) {
  # load the SQL pull data, update annually
  data <- read.csv(paste0(
    input_path,
    "VLAP_alldata2025.csv"
  ))

  ## DEFINING OBJECTS ----------------------------------------------------------

  # define the parameters relevant for VLAP
  params_keep <- c(
    "ALKALINITY, CARBONATE AS CACO3",
    "ALKALINITY, TOTAL",
    "APPARENT COLOR",
    "CHLORIDE",
    "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN",
    "ESCHERICHIA COLI",
    "GRAN ACID NEUTRALIZING CAPACITY",
    "PH",
    "PHOSPHORUS AS P",
    "SECCHI DISK TRANSPARENCY",
    "SPECIFIC CONDUCTANCE",
    "TURBIDITY"
  )

  # stations where CHL is composite but all other params are epi
  epi_override_stations <- c(
    "SUNSUN010",
    "SUNSUN020",
    "SUNSUN030",
    "SUNSUN040",
    "SUNSUN050",
    "SUNSUN060",
    "SUNSUN070",
    "SUNSUN080",
    "SUNSUN090",
    "SUNSUN1001",
    "SUNSUN110"
  )

  # map lakes with old station IDs to standardized IDs and names
  station_map <- list(
    STATIONID = c(
      "ROCHLSVLAPD",
      "ROCHLSD",
      "PEMMERVLAPD",
      "PEMMERD",
      "SPEGROVLAPD",
      "SPEGROD"
    ),
    new_id = c(
      "ROCHLSVLAPD",
      "ROCHLSVLAPD",
      "PEMMERVLAPD",
      "PEMMERVLAPD",
      "SPEGROVLAPD",
      "SPEGROVLAPD"
    ),
    statname = c(
      "ROCKY POND-DEEP SPOT",
      "ROCKY POND-DEEP SPOT",
      "PEMIGEWASSET LAKE-DEEP SPOT",
      "PEMIGEWASSET LAKE-DEEP SPOT",
      "SPECTACLE POND-DEEP SPOT",
      "SPECTACLE POND-DEEP SPOT"
    )
  )

  # Define depth-specific parameter codes
  depth_params <- list(
    alk = c(
      "EPILIMNION" = "alk_epi",
      "METALIMNION" = "alk_meta",
      "HYPOLIMNION" = "alk_hypo"
    ),
    chl = c(
      "COMPOSITE" = "CHL_comp"
    ),
    SPCD = c(
      "EPILIMNION" = "SPCD_epi",
      "METALIMNION" = "SPCD_meta",
      "HYPOLIMNION" = "SPCD_hypo"
    ),
    PH = c(
      "EPILIMNION" = "PH_epi",
      "METALIMNION" = "PH_meta",
      "HYPOLIMNION" = "PH_hypo"
    ),
    TP = c(
      "EPILIMNION" = "TP_epi",
      "METALIMNION" = "TP_meta",
      "HYPOLIMNION" = "TP_hypo"
    ),
    color = c(
      "EPILIMNION" = "color_epi",
      "METALIMNION" = "color_meta",
      "HYPOLIMNION" = "color_hypo"
    ),
    turb = c(
      "EPILIMNION" = "turb_epi",
      "METALIMNION" = "turb_meta",
      "HYPOLIMNION" = "turb_hypo"
    ),
    chloride = c(
      "EPILIMNION" = "chloride_epi",
      "METALIMNION" = "chloride_meta",
      "HYPOLIMNION" = "chloride_hypo"
    )
  )

  lake_depths <- c("EPILIMNION", "METALIMNION", "HYPOLIMNION", "COMPOSITE")

  ## DATA CLEANING ----------------------------------------------------------

  data <- data |>
    filter(VALID != "N", RESULTSTATUS == "FINAL")

  data_long <- data |>
    select(
      STATIONID,
      STATNAME,
      TOWN,
      RELLAKE,
      DEPTHZONE,
      DEPTH,
      STARTDATE,
      WSHEDPARMNAME,
      NUMRESULT,
      TEXTRESULT,
      QUALIFIER,
      DETLIM,
      ANALYTICALMETHOD,
      CURRENT_TROPHIC_STATUS,
      BEST_TROPHIC_CLASS,
      ACTCMTS,
      RESULTCMT
    ) |>
    filter(
      WSHEDPARMNAME %in% params_keep,
      !str_detect(STATIONID, "-GEN")
    ) |>
    mutate(
      # standardize station IDs and names
      idx = match(STATIONID, station_map$STATIONID),
      STATIONID = ifelse(!is.na(idx), station_map$new_id[idx], STATIONID),
      STATNAM = ifelse(
        !is.na(idx),
        station_map$statname[idx],
        str_trim(toupper(STATNAME))
      ),

      # clean depth zones
      DEPTHZONE = na_if(trimws(DEPTHZONE), ""),
      DEPTHZONE = case_when(
        DEPTHZONE == "UPPER" ~ "EPILIMNION",
        DEPTHZONE == "LOWER" ~ "HYPOLIMNION",
        TRUE ~ DEPTHZONE
      ),

      # ---- SUNSUN OVERRIDE LOGIC ------------------------------------------
      DEPTHZONE = case_when(
        STATIONID %in%
          epi_override_stations &
          WSHEDPARMNAME == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" ~
          "COMPOSITE",

        STATIONID %in%
          epi_override_stations &
          WSHEDPARMNAME != "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" ~
          "EPILIMNION",

        TRUE ~ DEPTHZONE
      ),
      # --------------------------------------------------------------------

      # assign param_depth
      param_depth = case_when(
        # Chlorophyll
        WSHEDPARMNAME == "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" &
          DEPTHZONE == "COMPOSITE" ~ depth_params$chl["COMPOSITE"],

        # Alkalinity
        (grepl("ALKALINITY", WSHEDPARMNAME, ignore.case = TRUE) |
          WSHEDPARMNAME == "GRAN ACID NEUTRALIZING CAPACITY") &
          DEPTHZONE %in% names(depth_params$alk) ~
          depth_params$alk[DEPTHZONE],

        # Secchi
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" &
          ANALYTICALMETHOD == "SECCHI-SCOPE" ~ "SECCHI",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" &
          ANALYTICALMETHOD != "SECCHI-SCOPE" ~ "SECCHI_NVS",

        # Depth-resolved lake parameters
        WSHEDPARMNAME == "SPECIFIC CONDUCTANCE" &
          DEPTHZONE %in% names(depth_params$SPCD) ~ depth_params$SPCD[
          DEPTHZONE
        ],
        WSHEDPARMNAME == "PH" &
          DEPTHZONE %in% names(depth_params$PH) ~ depth_params$PH[DEPTHZONE],
        WSHEDPARMNAME == "PHOSPHORUS AS P" &
          DEPTHZONE %in% names(depth_params$TP) ~ depth_params$TP[DEPTHZONE],
        WSHEDPARMNAME == "APPARENT COLOR" &
          DEPTHZONE %in% names(depth_params$color) ~ depth_params$color[
          DEPTHZONE
        ],
        WSHEDPARMNAME == "TURBIDITY" &
          DEPTHZONE %in% names(depth_params$turb) ~ depth_params$turb[
          DEPTHZONE
        ],
        WSHEDPARMNAME == "CHLORIDE" &
          DEPTHZONE %in% names(depth_params$chloride) ~ depth_params$chloride[
          DEPTHZONE
        ],

        # Tributaries
        (is.na(DEPTHZONE) | !(DEPTHZONE %in% lake_depths)) &
          !grepl("DEEP", STATNAME, ignore.case = TRUE) &
          WSHEDPARMNAME == "SPECIFIC CONDUCTANCE" ~ "SPCD_trib",
        (is.na(DEPTHZONE) | !(DEPTHZONE %in% lake_depths)) &
          !grepl("DEEP", STATNAME, ignore.case = TRUE) &
          WSHEDPARMNAME == "PH" ~ "PH_trib",
        (is.na(DEPTHZONE) | !(DEPTHZONE %in% lake_depths)) &
          !grepl("DEEP", STATNAME, ignore.case = TRUE) &
          WSHEDPARMNAME == "PHOSPHORUS AS P" ~ "TP_trib",
        (is.na(DEPTHZONE) | !(DEPTHZONE %in% lake_depths)) &
          !grepl("DEEP", STATNAME, ignore.case = TRUE) &
          WSHEDPARMNAME == "APPARENT COLOR" ~ "color_trib",
        (is.na(DEPTHZONE) | !(DEPTHZONE %in% lake_depths)) &
          !grepl("DEEP", STATNAME, ignore.case = TRUE) &
          WSHEDPARMNAME == "TURBIDITY" ~ "turb_trib",
        (is.na(DEPTHZONE) | !(DEPTHZONE %in% lake_depths)) &
          !grepl("DEEP", STATNAME, ignore.case = TRUE) &
          WSHEDPARMNAME == "CHLORIDE" ~ "chloride_trib",
        (is.na(DEPTHZONE) | !(DEPTHZONE %in% lake_depths)) &
          !grepl("DEEP", STATNAME, ignore.case = TRUE) &
          WSHEDPARMNAME == "ESCHERICHIA COLI" ~ "ecoli_trib",
        (is.na(DEPTHZONE) | !(DEPTHZONE %in% lake_depths)) &
          !grepl("DEEP", STATNAME, ignore.case = TRUE) &
          WSHEDPARMNAME %in%
            c(
              "ALKALINITY, TOTAL",
              "ALKALINITY, CARBONATE AS CACO3",
              "GRAN ACID NEUTRALIZING CAPACITY"
            ) ~ "alk_trib",

        TRUE ~ NA_character_
      ),

      # numeric handling (unchanged)
      NUMRESULT = case_when(
        param_depth %in%
          c("TP_epi", "TP_meta", "TP_hypo", "TP_trib") &
          (QUALIFIER == "<" | TEXTRESULT == "ND" | NUMRESULT < 0.005) ~ 0.0025,
        QUALIFIER == "<" & !is.na(DETLIM) & NUMRESULT <= DETLIM ~ DETLIM / 2,
        QUALIFIER == "<" & is.na(DETLIM) ~ NUMRESULT / 2,
        TRUE ~ NUMRESULT
      ),

      NUMRESULT = if_else(
        param_depth %in% c("TP_epi", "TP_meta", "TP_hypo", "TP_trib"),
        NUMRESULT * 1000,
        NUMRESULT
      ),

      STARTDATE = as.Date(STARTDATE, format = "%d-%b-%y"),
      year = lubridate::year(STARTDATE)
    ) |>
    # DROP Sunapee samples where they samples "1m off the bottom"
    filter(
      !(str_detect(STATIONID, "SUNSUN") &
        str_detect(STATNAM, "DEEP") &
        WSHEDPARMNAME == "PHOSPHORUS AS P" &
        is.na(DEPTHZONE))
    ) |>
    (\(df) df[df$RELLAKE %in% df$RELLAKE[df$year == 2025], ])()

  ## PIVOT WIDER FOR DATA ANALYSIS ----------------------------------------------------------
  data_wide <- data_long |>
    select(
      RELLAKE,
      STATIONID,
      TOWN,
      STATNAM,
      STARTDATE,
      param_depth,
      NUMRESULT,
      DEPTHZONE
    ) |>
    filter(!is.na(param_depth)) |>
    group_by(RELLAKE, STATIONID, TOWN, STATNAM, STARTDATE, param_depth) |>
    summarise(NUMRESULT = mean(NUMRESULT, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(
      id_cols = c(RELLAKE, STATIONID, TOWN, STATNAM, STARTDATE),
      names_from = param_depth,
      values_from = NUMRESULT
    ) |>
    select(where(~ !all(is.na(.x)))) |>
    mutate(
      STARTDATE = as.Date(STARTDATE, format = "%d-%b-%y"),
      year = year(STARTDATE)
    ) |>
    (\(df) df[df$RELLAKE %in% df$RELLAKE[df$year == 2025], ])()

  # calculate annual median per parameter per station
  data_year_median <- data_wide |>
    pivot_longer(
      cols = -c(RELLAKE, STATIONID, TOWN, STATNAM, STARTDATE, year),
      names_to = "parameter",
      values_to = "value"
    ) |>
    group_by(RELLAKE, STATIONID, TOWN, STATNAM, year, parameter) |>
    summarise(
      value = median(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_wider(
      id_cols = c(RELLAKE, STATIONID, TOWN, STATNAM, year),
      names_from = parameter,
      values_from = value
    ) |>
    mutate(
      # if annual TP median is less then 5.0 ug/L (because median was calculated using above and below DL values),
      # make the median be 2.5 ug/L
      TP_epi = if_else(!is.na(TP_epi) & TP_epi < 5, 2.5, TP_epi),
      TP_meta = if_else(!is.na(TP_meta) & TP_meta < 5, 2.5, TP_meta),
      TP_hypo = if_else(!is.na(TP_hypo) & TP_hypo < 5, 2.5, TP_hypo),
      TP_trib = if_else(!is.na(TP_trib) & TP_trib < 5, 2.5, TP_trib)
    )

  ## FILTER TO START YEARS FOR PLOTTING/AESTHETIC PURPOSES ONLY (SEPARATE DF) ----------------------------------------------------------
  lake_start_years <- tibble::tibble(
    STATIONID = c(
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
    lake_name = c(
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
    lake_town = c(
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

  data_plot <- data_year_median |>
    left_join(lake_start_years, by = "STATIONID") |>
    # keep all stations; only filter by start_year if it exists
    filter(is.na(start_year) | year >= start_year)

  ## RETURN DFs ----------------------------------------------------------
  return(list(
    data_long = data_long,
    data_wide = data_wide,
    data_year_median = data_year_median,
    data_plot = data_plot
  ))
}
