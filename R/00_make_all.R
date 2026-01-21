# Load environment variables (if .env exists)
if (file.exists(".env")) {
  if (!requireNamespace("dotenv", quietly = TRUE)) {
    install.packages("dotenv")
  }
  library(dotenv)
  load_dot_env(".env")
}

# Libraries
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
  "DBI",
  "odbc",
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
# Paths from .env
# ==========================
project_path <- Sys.getenv("PROJECT_PATH")
input_path <- Sys.getenv("INPUT_PATH")
output_path <- Sys.getenv("OUTPUT_PATH")
reg_path <- Sys.getenv("REG_PATH")
table_path <- Sys.getenv("TABLE_PATH")
template_path <- Sys.getenv("TEMPLATE_PATH")
mk_path <- Sys.getenv("MK_PATH")
report_path <- Sys.getenv("REPORT_PATH")

# ==========================
# Source helper scripts
# ==========================
source(file.path(project_path, "R", "theme.R"))
source(file.path(project_path, "R", "01_DBConnect.R"))
source(file.path(project_path, "R", "02_data_reformat.R"))
source(file.path(project_path, "R", "03_mannkendall.R"))
source(file.path(project_path, "R", "04_chl_tp_secchi.R"))
source(file.path(project_path, "R", "05_pH_cond.R"))
source(file.path(project_path, "R", "06_temp_DO.R"))
source(file.path(project_path, "R", "07_plankton.R"))
source(file.path(project_path, "R", "08_CYA_table.R"))
source(file.path(project_path, "R", "09_report_gen.R"))

#source(file.path(project_path, "R", "03_mannkendallNADA.R"))
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

data_long <- processed$data_long
data_plot <- processed$data_plot
data_year_median <- processed$data_year_median

# ==========================
# Mann-Kendall and Sen's Slope analysis
# ==========================
message("Running Mann-Kendall...")
run_vlap_mannkendall(data_year_median, mk_path, table_path)
#run_vlap_mannkendallNADA2(REG_NADA, mk_path, table_path)

# ==========================
# Plot generation
# ==========================

# imports Calibri font, need to run only once
#font_import(pattern = "calibri", prompt = FALSE)

message("Generating plots...")
make_chl_tp_secchi(
  data_plot,
  input_path,
  file.path(output_path, "chl_tp_secchi")
)

make_pH_conduc(
  data_plot,
  input_path,
  file.path(output_path, "pH_conduc")
)

make_temp_DO(input_path, file.path(output_path, "temp_DO"))
make_plankton(file.path(output_path, "plankton"))

message("All plots completed.")

# ==========================
# CYA table exports
# ==========================
message("Exporting CYA tables...")
make_CYA_table(data_long, table_path, input_path)
message("All tables exported.")

# ==========================
# generate reports
# ==========================
message("Starting report generation...")
report_gen(input_path, report_path, template_path)
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
