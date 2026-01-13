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
library(png)
library(grid)
library(ggnewscale)
library(scales)
library(Kendall)
library(trend)
library(NADA)

# ==========================
# Paths from .env
# ==========================
project_path <- Sys.getenv("PROJECT_PATH")
input_path <- Sys.getenv("INPUT_PATH")
output_path <- Sys.getenv("OUTPUT_PATH")
reg_path <- Sys.getenv("REG_PATH")
table_path <- Sys.getenv("TABLE_PATH")
reportgen_path <- Sys.getenv("TEMPLATE_PATH")
mk_path <- Sys.getenv("MK_PATH")

# ==========================
# Source helper scripts
# ==========================
source(file.path(project_path, "R", "theme.R"))
source(file.path(project_path, "R", "01_DBConnect.R"))
source(file.path(project_path, "R", "02_data_reformat.R"))
source(file.path(project_path, "R", "03_mannkendall.R"))
source(file.path(project_path, "R", "03-5_mannkendallNADA.R"))
source(file.path(project_path, "R", "04_chl_tp_secchi.R"))
source(file.path(project_path, "R", "05_pH_cond.R"))
source(file.path(project_path, "R", "06_temp_DO.R"))
source(file.path(project_path, "R", "07_plankton.R"))
source(file.path(project_path, "R", "08_CYA_table.R"))
source(file.path(project_path, "R", "09_report_gen.R"))
# source(file.path(project_path, "R", "chloride.R"))
# source(file.path(project_path, "R", "statcompare.R"))
# source(file.path(project_path, "R", "regression-OLD.R"))

# ========================================
# Database connection to pull newest data
# ========================================
# uncomment when new data need to be pulled from EMD
# message("Opening DB connection...")
# db_res <- DBConnect(
#   dsn = "DESPRD",
#   input_path = input_path
# )
# con <- db_res$con
# dbDisconnect(con)
# message("Database connection closed.")

# ====================
# reformatting data
# ====================
message("Reformatting data...")
processed <- data_reformat(input_path)

BTC <- processed$BTC
REG_plot <- processed$REG_plot
REG_MK <- processed$REG_MK
REG_NADA <- processed$REG_NADA
CYA_2025 <- processed$CYA_2025
CYA_long <- processed$CYA_long
LAKEMAP <- processed$LAKEMAP
PLANKTON <- processed$PLANKTON

# ==========================
# Mann-Kendall and Sen's SLope analysis
# ==========================
message("Running Mann-Kendall...")
#run_vlap_mannkendall(REG_MK, mk_path, table_path)
run_vlap_mannkendallNADA2(REG_NADA, mk_path, table_path)

# ==========================
# Plot generation
# ==========================
message("Generating plots...")
make_chl_tp_secchi(input_path, file.path(output_path, "chl_tp_secchi"))
make_pH_conduc(input_path, file.path(output_path, "pH_conduc"))
make_temp_DO(input_path, file.path(output_path, "temp_DO"))
make_plankton(PLANKTON, file.path(output_path, "plankton"))
message("All plots completed.")

# ==========================
# CYA table exports
# ==========================
message("Exporting CYA tables...")
make_CYA_table(CYA_2025, LAKEMAP, table_path, input_path)
message("All tables exported.")

# ==========================
# generate reports
# ==========================
message("Starting report generation...")
report_gen()
message("All reports generated.")

#not currently using below this line:
# ==========================
# Regression analysis- USE MK INSTEAD NOW
# ==========================
# message("Running regressions...")
# run_vlap_regressions(REG, reg_path, table_path)

# ==========================
# Chloride plot generation
# ==========================
# message("Generating plots...")
# make_chloride(input_path, file.path(output_path, "chloride"))

# comparison_results <- compare_reg_mk_trends(
#   tables_path = "tables",
#   output_path = "tables"
# )
