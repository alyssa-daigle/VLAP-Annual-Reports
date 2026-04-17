# ==========================
# Load environment variables FIRST
# ==========================
if (file.exists(".env")) {
  if (!requireNamespace("dotenv", quietly = TRUE)) {
    install.packages("dotenv")
  }
  library(dotenv)
  load_dot_env(".env")
}

# ==========================
# NOW load config (creates PROJECT_PATH, YEAR, etc.)
# ==========================
source("config/config.R") # <-- FIXED (no PROJECT_PATH used here)

# ==========================
# Libraries
# ==========================
libs <- c(
  "DBI",
  "odbc",
  "dotenv",
  "ggplot2",
  "dplyr",
  "readxl",
  "tidyr",
  "tibble",
  "parallel",
  "forcats",
  "cowplot",
  "lubridate",
  "purrr",
  "broom",
  "readr",
  "fs",
  "stringr",
  "magick",
  "png",
  "grid",
  "ggnewscale",
  "scales",
  "trend",
  "NADA",
  "extrafont",
  "rmarkdown"
)

invisible(lapply(libs, library, character.only = TRUE))

# ==========================
# Source helper scripts
# ==========================
source(file.path(PROJECT_PATH, "R", "theme.R"))
source(file.path(PROJECT_PATH, "R", "01_data_reformat.R"))
source(file.path(PROJECT_PATH, "R", "02_mannkendall.R"))
source(file.path(PROJECT_PATH, "R", "03_chl_tp_secchi.R"))
source(file.path(PROJECT_PATH, "R", "04_pH_cond.R"))
source(file.path(PROJECT_PATH, "R", "05_temp_DO.R"))
source(file.path(PROJECT_PATH, "R", "06_plankton.R"))
source(file.path(PROJECT_PATH, "R", "07_CYA_table.R"))
source(file.path(PROJECT_PATH, "R", "08_report_gen.R"))

# ====================
# Reformatting data
# ====================
message("Reformatting data...")
processed <- data_reformat(INPUT_PATH)

data_long <- processed$data_long
data_wide <- processed$data_wide
data_year_median <- processed$data_year_median
data_plot <- processed$data_plot

# ==========================
# Mann-Kendall
# ==========================
message("Running Mann-Kendall...")
run_vlap_mannkendall(data_year_median, MK_PATH, TABLE_PATH)
message("Done running Mann-Kendall.")

# ==========================
# Plot generation
# ==========================
message("Generating plots...")

make_chl_tp_secchi(
  data_plot,
  INPUT_PATH,
  file.path(OUTPUT_PATH, "chl_tp_secchi")
)

make_pH_conduc(
  data_plot,
  INPUT_PATH,
  file.path(OUTPUT_PATH, "pH_conduc")
)

make_temp_DO(
  INPUT_PATH,
  file.path(OUTPUT_PATH, "temp_DO")
)

make_plankton(
  INPUT_PATH = INPUT_PATH,
  OUTPUT_PATH = file.path(OUTPUT_PATH, "plankton")
)

message("All plots completed.")

# ==========================
# CYA tables
# ==========================
message("Exporting CYA tables...")
CYA_updated <- make_CYA_table(data_long, TABLE_PATH, INPUT_PATH)
message("All tables exported.")

# ==========================
# Reports
# ==========================
message("Starting report generation...")
report_gen()
message("All reports generated.")
