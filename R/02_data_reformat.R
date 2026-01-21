data_reformat <- function(input_path) {
  # load the SQL pull data, update annually
  data <- read_csv("C:/Users/alyssa.n.daigle/Desktop/testSQLpull.csv")

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

  # map old station IDs to standardized IDs and names
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

  # map depth zones to parameter-specific names
  depth_params <- list(
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

  # clean and filter the raw data
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
      ANALYTICALMETHOD,
      CURRENT_TROPHIC_STATUS,
      BEST_TROPHIC_CLASS,
      ACTCMTS,
      ACTIVE
    ) |>
    filter(WSHEDPARMNAME %in% params_keep, ACTIVE != "N") |>
    mutate(
      # standardize station IDs and names
      idx = match(STATIONID, station_map$STATIONID),
      STATIONID = ifelse(!is.na(idx), station_map$new_id[idx], STATIONID),
      STATNAM = ifelse(
        !is.na(idx),
        station_map$statname[idx],
        str_trim(toupper(STATNAME))
      ),

      # assign a param_depth value for each measurement
      param_depth = case_when(
        WSHEDPARMNAME ==
          "CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN" ~ "CHL_comp",
        grepl("ALKALINITY", WSHEDPARMNAME, ignore.case = TRUE) |
          WSHEDPARMNAME == "GRAN ACID NEUTRALIZING CAPACITY" ~ "alk_epi",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" &
          ANALYTICALMETHOD == "SECCHI-SCOPE" ~ "SECCHI",
        WSHEDPARMNAME == "SECCHI DISK TRANSPARENCY" &
          ANALYTICALMETHOD != "SECCHI-SCOPE" ~ "SECCHI_NVS",
        WSHEDPARMNAME == "SPECIFIC CONDUCTANCE" ~ depth_params$SPCD[DEPTHZONE],
        WSHEDPARMNAME == "PH" ~ depth_params$PH[DEPTHZONE],
        WSHEDPARMNAME == "PHOSPHORUS AS P" ~ depth_params$TP[DEPTHZONE],
        WSHEDPARMNAME == "APPARENT COLOR" ~ depth_params$color[DEPTHZONE],
        WSHEDPARMNAME == "TURBIDITY" ~ depth_params$turb[DEPTHZONE],
        WSHEDPARMNAME == "CHLORIDE" ~ depth_params$chloride[DEPTHZONE],
        WSHEDPARMNAME == "ESCHERICHIA COLI" ~ "ecoli",
        TRUE ~ NA_character_
      ),

      # adjust numeric results:
      # - half the value if flagged as "<"
      # - for TP, set NUMRESULT to 0.0025 if TEXTRESULT == "ND"
      # - convert phosphorus to µg/L
      NUMRESULT = case_when(
        QUALIFIER == "<" ~ NUMRESULT / 2,
        param_depth %in%
          c("TP_epi", "TP_meta", "TP_hypo") &
          TEXTRESULT == "ND" ~ 0.0025,
        TRUE ~ NUMRESULT
      ),
      NUMRESULT = if_else(
        param_depth %in% c("TP_epi", "TP_meta", "TP_hypo"),
        NUMRESULT * 1000,
        NUMRESULT
      )
    )

  # summarize and reshape the data for analysis
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
    )

  # create a lookup table for lakes for plotting purposes
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

  # join plotting metadata and filter for years after the start year
  data_plot <- data_year_median |>
    left_join(lake_start_years, by = "STATIONID") |>
    # keep all stations; only filter by start_year if it exists
    filter(is.na(start_year) | year >= start_year)

  # return both dataframes in a list
  return(list(
    data_long = data_long,
    data_year_median = data_year_median,
    data_plot = data_plot
  ))
}
