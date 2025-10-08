DBConnect <- function(dsn = "DESPRD") {
  library(DBI)
  library(odbc)
  library(dotenv)

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

  # Run queries
  BTC_full <- dbGetQuery(con, BTC_QUERY)
  REG_long <- dbGetQuery(con, REG_QUERY)

  # Return list of dataframes and connection
  list(BTC_full = BTC_full, REG_long = REG_long, con = con)
}
