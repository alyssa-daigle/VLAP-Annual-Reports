# Load environment variables (if .env exists)
if (file.exists(".env")) {
  if (!requireNamespace("dotenv", quietly = TRUE)) {
    install.packages("dotenv")
  }
  library(dotenv)
  load_dot_env(".env")
}

# Libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(tibble)
library(parallel)
library(forcats)
library(cowplot)
library(DBI)
library(odbc)
library(lubridate)
library(purrr)
library(broom)
library(readr)
library(fs)
library(stringr)
library(magick)

# ==========================
# Paths from .env
# ==========================
project_path <- Sys.getenv("PROJECT_PATH")
input_path <- Sys.getenv("INPUT_PATH")
output_path <- Sys.getenv("OUTPUT_PATH")
reg_path <- Sys.getenv("REG_PATH")
table_path <- Sys.getenv("TABLE_PATH")
reportgen_path <- Sys.getenv("TEMPLATE_PATH")

# ==========================
# Source helper scripts
# ==========================
source(file.path(project_path, "R", "theme.R"))
source(file.path(project_path, "R", "01_DBConnect.R"))
source(file.path(project_path, "R", "02_data_reformat.R"))
source(file.path(project_path, "R", "03_regression.R"))
source(file.path(project_path, "R", "04_chl_tp_secchi.R"))
source(file.path(project_path, "R", "05_pH_cond.R"))
source(file.path(project_path, "R", "06_temp_DO.R"))
source(file.path(project_path, "R", "07_plankton.R"))
source(file.path(project_path, "R", "08_CYA_table.R"))
source(file.path(reportgen_path, "report_gen.R"))

# ==========================
# Database + data prep
# ==========================
message("Opening DB connection...")
db_res <- DBConnect()
con <- db_res$con

message("Reformatting data...")
processed <- data_reformat(db_res$BTC_full, db_res$REG_long, db_res$CYA_full)
BTC <- processed$BTC
REG <- processed$REG
CYA <- processed$CYA

dbDisconnect(con)
message("Database connection closed.")

# ==========================
# Regression analysis
# ==========================
message("Running regressions...")
run_vlap_regressions(REG, reg_path, table_path)

# ==========================
# Plot generation
# ==========================
message("Generating plots...")
make_chl_tp_secchi(input_path, file.path(output_path, "chl_tp_secchi"))
make_pH_conduc(input_path, file.path(output_path, "pH_conduc"))
make_temp_DO(input_path, file.path(output_path, "temp_DO"))
make_plankton(input_path, file.path(output_path, "plankton"))
message("All plots completed.")

# ==========================
# CYA table exports
# ==========================
message("Exporting CYA tables...")
make_CYA_table(CYA, table_path)
message("All tables exported.")

# ==========================
# generate reports
# ==========================
message("Starting report generation...")
report_gen()
message("All reports generated.")
