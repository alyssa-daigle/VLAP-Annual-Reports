# VLAP Annual Report Plotting Scripts

This repository contains scripts used to streamline the Annual Report workflow by generating all plots using a single script.

## Main Plotting Script

-   **`make_all.R`**: The main "conglomerate" plotting script that can be run to generate all plots at once.

## Plot Functions

The individual scripts called by `make_all.R` include:

-   **`chl_tp_secchi.R`**: Contains the function to generate historical Chlorophyll-a, Total Phosphorus, and Secchi Disk transparency plots for each station.
-   **`ph_cond.R`**: Contains the function to generate historical pH and Conductivity plots for each station.
-   **`temp_DO.R`**: Contains the function to generate seasonal Temperature/Dissolved Oxygen profile plots for each station.
-   **`plankton.R`**: Contains the function to generate seasonal Phytoplankton Population plots for each station.

## Theming

-   **`theme.R`**: Contains theming functions for each plot type.

## Report Generation

**`report_gen.R`** (in progress) will be able to pull all the plots and tables (also in progress) into one report per active VLAP sampling station.