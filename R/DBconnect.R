DBConnect <- function(dsn = "DESPRD") {
  # Load environment variables
  if (file.exists(".env")) {
    if (!requireNamespace("dotenv", quietly = TRUE)) {
      install.packages("dotenv")
    }
    load_dot_env(".env")
  }

  # Connect using env vars
  con <- dbConnect(
    odbc::odbc(),
    dsn = dsn,
    uid = Sys.getenv("USERID"),
    pwd = Sys.getenv("PASSWORD")
  )

  # --- Queries ---

  # grabbing data for "Best Trophic Class"
  BTC_QUERY <- "
    SELECT 
      WQD_STATION.RELLAKE,
      WQD_STATION.RELLAKE_WBID,
      WQD_WATERBODY.WATERBODYID,
      WQD_WATERBODY.WATERBODYNAME,
      WQD_WATERBODY.BEST_TROPHIC_CLASS
    FROM DBAWQD.WQD_STATION WQD_STATION
    LEFT JOIN DBAWQD.WQD_WATERBODY WQD_WATERBODY
      ON WQD_STATION.WATERBODYID = WQD_WATERBODY.WATERBODYID
    WHERE WQD_STATION.RELLAKE IS NOT NULL 
      AND WQD_WATERBODY.BEST_TROPHIC_CLASS IS NOT NULL 
      AND WQD_WATERBODY.WATERBODYID LIKE 'NH%'
    ORDER BY WQD_STATION.RELLAKE
  "

  # grabbing data from the regression table
  REG_QUERY <- "
    SELECT DISTINCT 
        RELLAKE, RELLAKE_WBID, TOWN, STATIONID, STATNAME, STARTDATE, 
        WSHEDPARMNAME, NUMRESULT, QUALIFIER, RESULTUNITS, TEXTRESULT, 
        DEPTHZONE, ANALYTICALMETHOD, PROJID, DETLIM, VALID
    FROM WQD_REPORT_VIEW
    WHERE (
        WSHEDPARMNAME = 'CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN'
        AND RESULTUNITS IN ('UG/L', 'MG/M3')
        AND PROJID = 'VLAP'
        AND STATNAME LIKE '%DEEP%'
        AND (VALID = 'Y' OR VALID IS NULL)
        AND NOT (DEPTHZONE IN ('UPPER','LOWER','METALIMNION','SURFACE'))
        AND STARTDATE > TO_DATE('12/31/1984','MM/DD/YYYY')
    )
    OR (
        WSHEDPARMNAME = 'SPECIFIC CONDUCTANCE'
        AND RESULTUNITS IN ('UMHO/CM','US/CM')
        AND PROJID = 'VLAP'
        AND STATNAME LIKE '%DEEP%'
        AND (VALID = 'Y' OR VALID IS NULL)
        AND DEPTHZONE = 'EPILIMNION'
        AND STARTDATE > TO_DATE('12/31/1984','MM/DD/YYYY')
    )
    OR (
        WSHEDPARMNAME = 'PH'
        AND PROJID = 'VLAP'
        AND STATNAME LIKE '%DEEP%'
        AND (VALID = 'Y' OR VALID IS NULL)
        AND DEPTHZONE = 'EPILIMNION'
        AND STARTDATE > TO_DATE('12/31/1984','MM/DD/YYYY')
    )
    OR (
        WSHEDPARMNAME = 'SECCHI DISK TRANSPARENCY'
        AND RESULTUNITS = 'M'
        AND PROJID = 'VLAP'
        AND STATNAME LIKE '%DEEP%'
        AND (VALID = 'Y' OR VALID IS NULL)
        AND STARTDATE > TO_DATE('12/31/1984','MM/DD/YYYY')
    )
    OR (
        WSHEDPARMNAME = 'PHOSPHORUS AS P'
        AND PROJID = 'VLAP'
        AND STATNAME LIKE '%DEEP%'
        AND (VALID = 'Y' OR VALID IS NULL)
        AND DEPTHZONE IN ('HYPOLIMNION','EPILIMNION')
        AND STARTDATE > TO_DATE('12/31/1984','MM/DD/YYYY')
    )
  "

  # run queries
  BTC_full <- dbGetQuery(con, BTC_QUERY)
  REG_long <- dbGetQuery(con, REG_QUERY)

  # restructure BTC DF
  trophic_map <- c(
    "OLIGOTROPHIC" = 1,
    "MESOTROPHIC" = 2,
    "EUTROPHIC" = 3
  )

  BTC <- BTC_full |>
    select(RELLAKE, BEST_TROPHIC_CLASS) |>
    rename(
      lake = RELLAKE,
      BTC = BEST_TROPHIC_CLASS
    ) |>
    mutate(
      BTC_num = trophic_map[BTC]
    ) |>
    group_by(lake) |>
    summarise(
      # pick the BTC name corresponding to the lowest numeric value
      BTC = BTC[which.min(BTC_num)],
      .groups = "drop"
    )

  # pivot REG_long to wide format
  REG <- REG_long |>
    filter(PROJID == "VLAP", DEPTHZONE == "EPILIMNION") |>
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
    pivot_wider(
      id_cols = c(RELLAKE, TOWN, STATIONID, STATNAME, STARTDATE, DEPTHZONE),
      names_from = WSHEDPARMNAME,
      values_from = NUMRESULT,
      values_fn = mean,
      values_fill = NA
    ) |>
    rename(
      lake = RELLAKE,
      town = TOWN,
      stationid = STATIONID,
      stationname = STATNAME,
      date = STARTDATE,
      depth = DEPTHZONE,
      chl = `CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN`,
      pH = PH,
      TP = `PHOSPHORUS AS P`,
      secchi = `SECCHI DISK TRANSPARENCY`,
      cond = `SPECIFIC CONDUCTANCE`
    )

  # force DFs to appear in global environment
  assign("BTC", BTC, envir = .GlobalEnv)
  assign("REG", REG, envir = .GlobalEnv)
  assign("con", con, envir = .GlobalEnv)
}
