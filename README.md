# VLAP Annual Report Generating Scripts

This repository contains scripts used to streamline the Annual Report workflow by performing regressions for trend analysis and generating all plots using a single script.

## Main Plotting Script

-   **`00_make_all.R`**: The main "conglomerate" plotting script that can be run to generate all plots at once.


## Functions

The individual scripts called by `make_all.R` include:

-   **`01_DBconnect.R`**: Connects to NHDES EMD to pull newest data.
-   **`02_data_reformat.R`**: Reformats data to rename columns.
-   **`03_regressions.R`**: Runs regression on EMD data to determine whether singificant parameter trends are increasing, decreasing, or stable.
-   **`04_chl_tp_secchi.R`**: Contains the function to generate historical Chlorophyll-a, Total Phosphorus, and Secchi Disk transparency plots for each station.
-   **`05_ph_cond.R`**: Contains the function to generate historical pH and Conductivity plots for each station.
-   **`06_temp_DO.R`**: Contains the function to generate seasonal Temperature/Dissolved Oxygen profile plots for each station.
-   **`07_plankton.R`**: Contains the function to generate seasonal Phytoplankton Population plots for each station.
-   **`08_CYA_table`**: Contains the function to generate Current Year Average tables for each lake.

## Theming

-   **`theme.R`**: Contains theming functions for each plot type.

## Report Generation

**`report_template.Rmd`**: Template markdown script that provides theming and placement of plots into a docx for the reports.
**`report_gen.R`**: Generates a report for each station by looping through a lake map. A docx is generated for each station that pulls together all plots and tables. Leaves room to manually enter observations and recommendations (typically lake-specific).
