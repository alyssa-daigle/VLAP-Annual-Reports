# VLAP Annual Report Plotting Scripts

This repository contains scripts used to streamline the Annual Report workflow by generating all plots using a single script.

## Main Script

- **`make_all.R`**: The main "conglomerate" script that can be run to generate all plots at once.

## Plot Functions

The individual scripts called by `make_all.R` include:

- **`chl_tp_secchi.R`**: Contains the function to generate historical Chlorophyll-a, Total Phosphorus, and Secchi Disk transparency plots for each station.  
- **`ph_cond.R`**: Contains the function to generate historical pH and Conductivity plots for each station.  
- **`temp_DO.R`**: Contains the function to generate seasonal Temperature/Dissolved Oxygen profile plots for each station.  

Note that a Seasonal Phytoplankton Relative Abundance script is coming soon. 

## Theming

- **`theme.R`**: Contains theming functions for each plot type.
