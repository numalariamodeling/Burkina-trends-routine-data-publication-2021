## For the paper titled

Quantifying trends in malaria incidence using routine case data in Burkina Faso in the presence of improved reporting and treatment-seeking


## Please refer questions about code to

Sebastian Rodriguez at
{sebastian@rodriguez.cr}


## NOTE

Please dont write any data if possible


## Usage

The "project folder" directory contains the scripts to clean the data as per explained in the methods section. Code in this folder will also run the analysis on the trend components, and create the main and supplemental text figures.

The "pre-processing data" directory contains code for cleaning data for outliers and recalculating u5 and ov5 counts from "lower level" raw data.


# "project folder"

1) Execute scripts in "data processing" directory to impute the data, find active/inactive health facilities, and calculate reporting rates.
1.1) Execute "HF_conf_rdt_and_allout_u5_imputation.R" scrip
1.2) Execute "making_HF_active.R" script
1.3) Execute "HF_to_DS_agg_imputed_conf_rdt_and_allout_with_rep_weights_AVG_activeHFs.R" script

2) Code in "analysis for figures" will run the analysis and create the figures in the main text and SI

3) The "for Ov5s" folder will do steps (1) and (2) for the Over-5 group



# Last edit

Dec 10, 2021