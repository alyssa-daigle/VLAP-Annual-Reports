# ==========================
# VLAP Automated Processing Script
# ==========================

# Load environment variables
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

# ==========================
# Paths from .env
# ==========================
project_path <- Sys.getenv("PROJECT_PATH")
input_path <- Sys.getenv("INPUT_PATH")
output_path <- Sys.getenv("OUTPUT_PATH")
reg_path <- Sys.getenv("REG_PATH")
table_path <- Sys.getenv("TABLE_PATH")

# ==========================
# Source helper scripts
# ==========================
source(file.path(project_path, "R", "01_DBConnect.R"))
source(file.path(project_path, "R", "02_data_reformat.R"))
source(file.path(project_path, "R", "03_regression.R"))
source(file.path(project_path, "R", "theme.R"))
source(file.path(project_path, "R", "04_chl_tp_secchi.R"))
source(file.path(project_path, "R", "05_pH_cond.R"))
source(file.path(project_path, "R", "06_temp_DO.R"))
source(file.path(project_path, "R", "07_plankton.R"))
source(file.path(project_path, "R", "08_CYA_table.R"))

# ==========================
# Logging wrapper
# ==========================
log_warnings <- function(expr, log_file) {
  warnings <- character()

  withCallingHandlers(
    {
      force(expr)
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (length(warnings) > 0) {
    message(
      paste0(warnings, collapse = "\n"),
      "\n",
      file = log_file,
      append = TRUE
    )
  }
}

# ==========================
# Setup log file
# ==========================
log_file <- file.path(output_path, "warnings_log.txt")
message(
  "=== VLAP Processing Started at ",
  Sys.time(),
  "===\n",
  file = log_file,
  append = TRUE
)

# ==========================
# Database + data prep
# ==========================
message("Opening DB connection\n")
db_res <- DBConnect() # returns list: BTC_full, REG_long, con
con <- db_res$con

message("Reformatting data\n")
processed <- data_reformat(db_res$BTC_full, db_res$REG_long, db_res$CYA_FULL)
BTC <- processed$BTC
REG <- processed$REG
CYA <- processed$CYA

# Close DB connection
dbDisconnect(con)
message("Database connection closed\n")

# ==========================
# Regression analysis
# ==========================
message("Starting regressions\n")
log_warnings(
  run_vlap_regressions(REG, reg_path, table_path),
  log_file
)
message("Done regressions\n")

# ==========================
# Plot generation
# ==========================
message("Starting plotting\n")

log_warnings(
  make_chl_tp_secchi(
    input_path = input_path,
    output_path = file.path(output_path, "chl_tp_secchi")
  ),
  log_file
)

log_warnings(
  make_pH_conduc(
    input_path = input_path,
    output_path = file.path(output_path, "pH_conduc")
  ),
  log_file
)

log_warnings(
  make_temp_DO(
    input_path = input_path,
    output_path = file.path(output_path, "temp_DO")
  ),
  log_file
)

log_warnings(
  make_plankton(
    input_path = input_path,
    output_path = file.path(output_path, "plankton")
  ),
  log_file
)

message("Done plotting\n")
message(
  "=== VLAP Processing Completed at",
  Sys.time(),
  "===\n",
  file = log_file,
  append = TRUE
)

# ==========================
# CYA table exports
# ==========================
message("Starting CYA table exports\n")

# Run the function with warning logging
log_warnings(
  make_CYA_table(CYA_wide, table_path),
  log_file
)

message("Done CYA table exports\n")
