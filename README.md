# VLAP Annual Report Generating Scripts

This repository contains scripts used to streamline the Annual Report workflow by performing regressions for trend analysis, generating all plots, and generating a report document for each VLAP sampling station using a single script.

## Main Plotting Script

-   **`00_make_all.R`**: The main "conglomerate" plotting script that can be run to generate all plots at once.

## Functions

The individual scripts called by `make_all.R` include:

-   **`01_DBconnect.R`**: Connects to NHDES EMD to pull recent data.
-   **`02_data_reformat.R`**: Reformats data to rename columns.
-   **`03_regressions.R`**: Runs regression on EMD data to determine whether significant parameter trends are increasing, decreasing, or stable.
-   **`04_chl_tp_secchi.R`**: Contains the function to generate historical Chlorophyll-a, Total Phosphorus, and Secchi Disk transparency plots for each station (uses data from EMD connection)
-   **`05_ph_cond.R`**: Contains the function to generate historical pH and Conductivity plots for each station (uses data from EMD connection).
-   **`06_temp_DO.R`**: Contains the function to generate seasonal Temperature/Dissolved Oxygen profile plots for each station (currently requires Excel data sheet).
-   **`07_plankton.R`**: Contains the function to generate seasonal Phytoplankton Population plots for each station (currently requires Excel data sheet).
-   **`08_CYA_table`**: Contains the function to generate Current Year Average tables for each lake.
-   **`report_gen.R`**: Contains the function to generate a report according to **`report_template.Rmd`** for each sampling station by looping through a lake map. A .docx is generated for each station that pulls together all plots and tables. Leaves room to manually enter observations and recommendations (typically lake-specific).

## Theming

-   **`theme.R`**: Contains theme functions for each plot type.