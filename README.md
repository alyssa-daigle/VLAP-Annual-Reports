# VLAP Annual Report Generating Scripts

This repository contains scripts used to streamline the Annual Report workflow by cleaning the data, performing Mann-Kendall for trend analysis, generating all plots, and generating a report document for each VLAP sampling station. All reports generated from this pipeline are ADA/WCAG compliant. To use each year, simply save the current year data to the **data** folder and update YEAR in the .env file.

## Repo Structure

-  **`R`**: Contains all of the R scripts necessary to generate the reports.
- **`config`**: Contains the config file, which sets all of the file paths and defines them in the environment.
- **`data`**: Contains all the data files needed to generate the reports. Data files need up be updated each year with most recent data. Note that some files are not on repo due to file size restrictions. See **Data Files** below.
- **`report_generation`**: Contains the Word Doc template and R markdown file to create the report documents.
- **`special-cases`**: Contains data and scripts for "special case" lakes (i.e., short time series).
- **`outputs`**: Not visible on repo. Contains a folder for each year. Within each year folder, contains the following folders:
    - **`FINAL`**: Where the final annual report PDFs are stored.
    - **`lake_excerpts`**: Where the Observations word docs are stored.
    - **`mannkendall`**: Where the statistical results of the Mann-Kendall analysis are stored.
    - **`plots`**: Where the plots for each station are stored, broken into separate folders for each plot type.
    - **`report_drafts_ADA`**: Where the assembled report drafts are stored, generated from adding the Observations to the report templates.
    - **`report-templates`**: Where the report templates are stored (Word Docs containing only titles/headers, data tables, and plots without personalized observations).
    - **`tables`**: Where the MK trend summary and CYA tables are stored.

## Main Script

-   **`00_make_all.R`**: The main "conglomerate" script that can be run to perform all data collection/reformating/analysis, plotting, and report generating at once.

## Functions

The individual scripts called by **`00_make_all.R`** include:

-   **`01_data_reformat.R`**: Reformats data to rename columns and make data handling easier.
-   **`02_mannkendall.R`**: Runs Mann-Kendall analysis to determine whether significant parameter trends are increasing, decreasing, or stable. Uses R package [**trend**](https://cran.r-project.org/web/packages/trend/index.html).
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

-   **`VLAP_alldata2025.csv`**: data generated from SQL EMD data pull; needs to be re-copied into this repo annually.
-   **`LAKEMAP.csv`**: provides a comprehensive list of all lakes and their stations participating in VLAP.
-   **`master-DO-2025.xlsm`**: copy of the master DO file; needs to be re-copied into this repo annually.
-   **`Historical_Phytoplankton_Data_Thru2025.xlsm`**: copy of the master phytoplankton file; needs to be re-copied into this repo annually.

