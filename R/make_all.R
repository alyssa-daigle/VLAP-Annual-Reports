if (file.exists("../.env")) {
  readRenviron("../.env")
}

path <- Sys.getenv("PATH")

library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(tibble)

source(paste0(path, "/R/theme.R"))
source(paste0(path, "/R/chl_tp_secchi.R"))

input_path <- paste0(path, "/data/")
output_path <- paste0(path, "/plots/chl_tp_secchi/")
make_chl_tp_secchi(
  input_path = paste0(path, "/data/"),
  output_path = paste0(path, "/plots/chl_tp_secchi/")
)
