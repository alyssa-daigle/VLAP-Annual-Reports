DBConnect <- function(dsn = "DESPRD", input_path) {
  library(DBI)
  library(odbc)
  library(dotenv)
  library(readr)

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

  REG_QUERY <- "
    SELECT DISTINCT 
        RELLAKE, RELLAKE_WBID, TOWN, STATIONID, STATNAME, STARTDATE, 
        WSHEDPARMNAME, NUMRESULT, QUALIFIER, RESULTUNITS, TEXTRESULT, 
        DEPTHZONE, ANALYTICALMETHOD, PROJID, DETLIM, VALID
    FROM WQD_REPORT_VIEW
    WHERE PROJID = 'VLAP'
  "

  CYA_QUERY <- "
    SELECT
      RELLAKE_WBID,
      RELLAKE,
      STATNAME,
      STATIONID,
      DEPTHZONE,
      TOWN,
      TO_CHAR(STARTDATE, 'YYYY') AS PYEAR,
      WSHEDPARMNAME,
      NUMRESULT,
      QUALIFIER,
      RESULTUNITS,
      TEXTRESULT,
      ANALYTICALMETHOD,
      DETLIM, 
      PROJID
    FROM
      WQD_REPORT_VIEW
    WHERE
      PROJID = 'VLAP'
      AND (VALID = 'Y' OR VALID IS NULL)
      AND WSHEDPARMNAME IN (
          'PH',
          'GRAN ACID NEUTRALIZING CAPACITY',
          'ALKALINITY, TOTAL',
          'ALKALINITY, CARBONATE AS CACO3',
          'CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN',
          'SPECIFIC CONDUCTANCE',
          'CONDUCTIVITY',
          'ESCHERICHIA COLI',
          'SECCHI DISK TRANSPARENCY',
          'TURBIDITY',
          'PHOSPHORUS AS P',
          'CHLORIDE',
          'APPARENT COLOR'
      )
  "

  # --- Run queries ---
  BTC_full <- dbGetQuery(con, BTC_QUERY)
  REG_long <- dbGetQuery(con, REG_QUERY)
  CYA_full <- dbGetQuery(con, CYA_QUERY)

  # --- Write CSVs ---
  write_csv(BTC_full, file.path(input_path, "BTC_full.csv"))
  write_csv(REG_long, file.path(input_path, "REG_long.csv"))
  write_csv(CYA_full, file.path(input_path, "CYA_full.csv"))

  # Return list of dataframes and connection
  list(
    BTC_full = BTC_full,
    REG_long = REG_long,
    CYA_full = CYA_full,
    con = con
  )
}
