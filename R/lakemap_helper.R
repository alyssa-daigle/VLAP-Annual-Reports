if (file.exists(".env")) {
  if (!requireNamespace("dotenv", quietly = TRUE)) {
    install.packages("dotenv")
  }
  library(dotenv)
  load_dot_env(".env")
}

input_path <- Sys.getenv("INPUT_PATH")

# Read data - SHOULD RE-DOWNLOADED EACH YEAR TO CATCH NEW STATIONS
lakemap_sqlpull <- read.csv(file.path(input_path, "LAKEMAP_SQLpull.csv"))

# Clean data: uppercase character columns, remove duplicates & NA WBID
lakemap_sqlpull_clean <- lakemap_sqlpull |>
  mutate(across(where(is.character), toupper)) |>
  distinct(STATIONID, .keep_all = TRUE) |>
  filter(!is.na(RELLAKE_WBID) & RELLAKE_WBID != "")

# ── Station ID lists by lake/station ─────────────────────────────
cobbettsSTN1 <- c(
  "COBWINSD",
  "COBWINMI",
  "COBWINMS",
  "COBWINFR",
  "COBWINTB",
  "COBWINHS",
  "COBWINO"
)
cobbettsSTN2 <- c(
  "COBWINND",
  "COBWINDW",
  "COBWINDE",
  "COBWINCC",
  "COBWINI",
  "COBWINARM",
  "COBWINWR",
  "COBWINBV",
  "COBWINCB111",
  "COBWINCBCVS",
  "COBWINCB",
  "COBWINCBW",
  "COBWINHC",
  "COBWINMESS",
  "COBWINCOM",
  "COBWINBDD",
  "COBWINSI"
)

greatN <- c("GRTKINND", "GRTKINB", "GRTKINT", "GRTKINSEC01")
greatS <- c(
  "GRTKINSD",
  "GRTKINO",
  "GRTKINK",
  "GRTKING",
  "GRTKINPAB",
  "GRTKINSEC02",
  "GRTKINSEC03",
  "GRTKINSEC04"
)

highlandN <- c(
  "HIGSTDND",
  "HIGSTDB",
  "HIGSTDN",
  "HIGSTDP",
  "HIGSTDPC2",
  "HIGSTDBBS",
  "HIGSTDRBT",
  "HIGSTDUBK"
)
highlandS <- c(
  "HIGSTDSD",
  "HIGSTDC",
  "HIGSTDDC",
  "HIGSTDCBP",
  "HIGSTDCBE",
  "HIGSTDCBW",
  "HIGSTDCB",
  "HIGSTDDB",
  "HIGSTDK",
  "HIGSTDR",
  "HIGSTDC",
  "HIGSTDA"
)

waukewanMAYO <- c(
  "WAUMERMD",
  "WAUMERCRT",
  "WAUMER9",
  "WAUMERI",
  "WAUMER6",
  "WAUMERP",
  "WAUMER7",
  "WAUMER7B",
  "WAUMER7A",
  "WAUMER10"
)
waukewanWINONA <- c(
  "WAUMERWD",
  "WAUMER1",
  "WAUMERO",
  "WAUMERMPC",
  "WAUMERRB",
  "WAUMER11A",
  "WAUMER11B",
  "WAUMER11C",
  "WAUMER12",
  "WAUMER13",
  "WAUMER14",
  "WAUMER15",
  "WAUMER17",
  "WAUMER18",
  "WAUMER19",
  "WAUMER2",
  "WAUMER2A",
  "WAUMER2B",
  "WAUMER2C",
  "WAUMER3",
  "WAUMER3A",
  "WAUMER3B",
  "WAUMER4",
  "WAUMER4A",
  "WAUMER4B",
  "WAUMER4C",
  "WAUMER5",
  "WAUMER5A",
  "WAUMER5B",
  "WAUMER5C",
  "WAUMER5D"
)

pawtuckN <- c(
  "PAWNOTND",
  "PAWNNOTDAM2",
  "PAWNOTF1",
  "PAWNOTF3",
  "PAWNOTW",
  "PAWNNOTB",
  "PAWNNOTF2",
  "PAWNNOTR",
  "PAWNNOTW",
  "PAWNNOTH",
  "PAWNNOTF1",
  "PAWNNOTF2",
  "PAWNNOTF3",
  "PAWNNOTNEC",
  "PAWNNOT18WG"
)
pawtuckS <- c(
  "PAWNOTSD",
  "PAWNNOTDAM1",
  "PAWSNOTJTR",
  "PAWSNOTM",
  "PAWNOTSPB",
  "PAWSNOTLV",
  "PAWSNOTMR",
  "PAWNNOT18IR",
  "PAWNNOT17SR"
)

sunsun_nearshore <- c(
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
sunsun_deep <- c("SUNSUN1D", "SUNSUN2D", "SUNSUN220D", "SUNSUN3D")

# ── Assign RELLAKE based on station IDs ──────────────────────────
lakemap_final <- lakemap_sqlpull_clean |>
  mutate(
    RELLAKE = case_when(
      # COBBETTS POND
      STATIONID %in% cobbettsSTN1 ~ "COBBETTS POND, STN 1",
      STATIONID %in% cobbettsSTN2 ~ "COBBETTS POND, STN 2",

      # PEA PORRIDGE POND
      str_starts(STATIONID, "PEABMAD") ~ "BIG PEA PORRIDGE POND",
      str_starts(STATIONID, "PEAMMAD") ~ "MIDDLE PEA PORRIDGE POND",

      # GREAT POND
      STATIONID %in% greatN ~ "GREAT POND, NORTH STN",
      STATIONID %in% greatS ~ "GREAT POND, SOUTH STN",

      # HIGHLAND LAKE
      STATIONID %in% highlandN ~ "HIGHLAND LAKE, NORTH STN",
      STATIONID %in% highlandS ~ "HIGHLAND LAKE, SOUTH STN",

      # LAKE WAUKEWAN
      STATIONID %in% waukewanMAYO ~ "LAKE WAUKEWAN, MAYO STN",
      STATIONID %in% waukewanWINONA ~ "LAKE WAUKEWAN, WINONA STN",
      str_starts(STATIONID, "SNANWHA") ~ "SNAKE RIVER - TO LAKE WAUKEWAN",

      # LAKE WINNISQUAM
      str_starts(STATIONID, "WINPLAC") ~ "LAKE WINNISQUAM, POT ISL",
      str_starts(STATIONID, "WINTLAC") ~ "LAKE WINNISQUAM, THREE ISL",
      str_starts(STATIONID, "WINMBEL") ~ "LAKE WINNISQUAM, MOHAWK ISL",

      # SPECIAL CASES
      str_starts(STATIONID, "ISLDER") ~ "BIG ISLAND POND",
      RELLAKE == "JACKMAN RESERVOIR" ~ "FRANKLIN PIERCE LAKE",

      # PAWTUCKAWAY LAKE
      STATIONID %in% pawtuckN ~ "PAWTUCKAWAY LAKE, NORTH STN",
      STATIONID %in% pawtuckS ~ "PAWTUCKAWAY LAKE, SOUTH STN",

      # SUNAPEE
      STATIONID %in% sunsun_nearshore ~ "SUNAPEE LAKE, NEARSHORE",
      STATIONID == "SUNSUN1D" ~ "SUNAPEE LAKE, STN 200",
      STATIONID == "SUNSUN2D" ~ "SUNAPEE LAKE, STN 210",
      STATIONID == "SUNSUN220D" ~ "SUNAPEE LAKE, STN 220",
      STATIONID == "SUNSUN3D" ~ "SUNAPEE LAKE, STN 230",
      str_starts(STATIONID, "SUNSUN") &
        !STATIONID %in%
          c(sunsun_nearshore, sunsun_deep) ~ "SUNAPEE LAKE TRIBUTARY",

      # DEFAULT
      TRUE ~ RELLAKE
    )
  ) |>
  # Exclude stations ending with -GEN
  filter(!str_ends(STATIONID, "-GEN")) |>
  arrange(RELLAKE)

# Write cleaned CSV
write.csv(
  lakemap_final,
  file.path(input_path, "LAKEMAP.csv"),
  row.names = FALSE
)

# CHECK WHICH STATIONS GOT MISSED

# lake_list <- c(
#   "COBBETTS POND",
#   "GREAT POND",
#   "HIGHLAND LAKE",
#   "LAKE WAUKEWAN",
#   "LAKE WINNISQUAM",
#   "PAWTUCKAWAY LAKE",
#   "SUNAPEE LAKE"
# )

# lakemap_lakes_df <- lakemap_final |>
#   filter(RELLAKE %in% lake_list)
