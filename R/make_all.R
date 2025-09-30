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

# Paths from .env
project_path <- Sys.getenv("PROJECT_PATH")
input_path   <- Sys.getenv("INPUT_PATH")
output_path  <- Sys.getenv("OUTPUT_PATH")

# Source helper scripts
source(file.path(project_path, "R", "theme.R"))
source(file.path(project_path, "R", "chl_tp_secchi.R"))

# Run functions
make_chl_tp_secchi(
  input_path  = input_path,
  output_path = file.path(output_path, "chl_tp_secchi")
)

make_pH_conduc(
  input_path  = input_path,
  output_path = file.path(output_path, "pH_conduc")
)
