# VLAP Annual Report Generating Scripts

This repository contains scripts used to streamline the Annual Report workflow by performing Mann-Kendall for trend analysis, generating all plots, and generating a report document for each VLAP sampling station using a single script.

## Main Script

-   **`00_make_all.R`**: The main "conglomerate" script that can be run to perform all data collection/reformating/analysis, plotting, and report generating at once.

## Functions

The individual scripts called by **`00_make_all.R`** include:

-   **`01_data_reformat.R`**: Reformats data to rename columns and make data handling easier.
-   **`02_mannkendall.R`**: Runs Mann-Kendall analysis to determine whether significant parameter trends are increasing, decreasing, or stable. Uses R package trend[!https://cran.r-project.org/web/packages/trend/index.html].
-   **`03_chl_tp_secchi.R`**: Contains the function to generate historical Chlorophyll-a, Total Phosphorus, and Secchi Disk transparency plots for each station. Annual medians plotted with Sen's Slope from MK test.
-   **`04_ph_cond.R`**: Contains the function to generate historical pH and Conductivity plots for each station. Annual medians plotted with Sen's Slope from MK test.
-   **`05_temp_DO.R`**: Contains the function to generate seasonal Temperature/Dissolved Oxygen profile plots for each station (currently requires master Excel data sheet).
-   **`06_plankton.R`**: Contains the function to generate seasonal Phytoplankton Population plots for each station (currently requires master Excel data sheet).
-   **`07_CYA_table.R`**: Contains the function to generate Current Year Average tables for each lake.
-   **`08_report_gen.R`**: Contains the function to generate a report according to **`report_template.Rmd`** for each sampling station by looping through a lake map. Uses **`template.docx`** as a template Word document, including NHDES headings and contact info. A .docx is generated for each station that pulls together all plots and tables. Leaves room to manually enter observations and recommendations (typically lake-specific).

## Themes

-   **`theme.R`**: Contains theme functions for each plot type.

## Data Files

Note that the master data file for all chemical data is stored locally due to its large sizes. Contact me to obtain files.

-   **`LAKEMAP.csv`**: provides a comprehensive list of all lakes and their stations participating in VLAP.
-   **`lookup.xlsx`**: acts as a map for lakes that have multiple deep spots and tributaries associated with different deep spots.
-   **`master-DO-2025.xlsm`**: copy of the master DO file; needs to be re-copied into this repo annually. Eventually can be a SQL pull. 
-   **`Historical_Phytoplankton_Data_Thru2025.xlsm`**: copy of the master phytoplankton file; needs to be re-copied into this repo annually.

