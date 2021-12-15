# Final-insect-trends-correlations
All reproducible data and scripts for the paper 'Long-term trends of insect taxa are only weakly correlated' By R van Klink, DE Bowler, KB Gongalsky and JM Chase 

Description of all files in this repository: 

Data files: (folder Data)
- Fulldata allgroups.rds
Dataframe used for the analysis in 'simple_correlations_brms_restgroups.R'. It was compiled using the script 'compile Data for analysis.R'

-Fulldata allorders.rds
Dataframe used for the analysis at the order level ('simple_correlations_brms.R'). It was compiled using the script 'compile Data for analysis.R'





R files
-compile Data for analysis.R 
This script compiles the data from the KNB repository and selects the data useful for the current analysis. Note that the data from Greenland need to be compiled separately (see below).  

- map.R 
Produces Fig S1

- map-preparation.R
Helper code to prepare the map to be built 

- plot results from brms models.R
This is the final analysis of the paper. It takes the output files from 'simple_correlations_brms.R' and performs all analyses and produces all plots and tables. 

- simple_correlations_brms.R
This analysis produces the input files for plotting. It calculates the trends of the different orders (listed in 'comparison_jobs.csv' and
'comparison_jobs_groups_less_good.csv') in Stage 1, and takes samples from the posterior distributions. It then calculates the correlations for each pair of taxa. All trends, samples and correlations are 
saved for use in 'plot results from brms models.R' 

- simple_correlations_brms_restgroups.R
As above but for the different common groupings (files: 'comparison_jobs_groups.csv' and 'comparison_jobs_groups_less_good.csv'). Is calculates the trends, 
and takes samples from the posterior distributions. It then calculates the correlations for each pair of taxa. All trends, samples and correlations are 
saved for use in 'plot results from brms models.R' 

- Greenland data processing
Because we are not allowed to publish derived products from this dataset, we provide the processing code, which can be used on the Zackenberg arthropod
data downloaded from www.g-e-m.dk




Other files: 
- array_submit_script_stan_roel_corr.txt.sh
Submit shell script for UFZ  HPC for all comparisons at order level 

- array_submit_script_stan_roel_corr_restgroups.txt.sh
Submit shell script for UFZ  HPC for all comparisons at the 'common grouping' level 

- basic_period_trend.stan
Stan code for running mixed effect model with 'period' as random effect

- basic_trend.stan
Stan code for running simple regression model without random effects

- comparison_jobs.csv
List of Order-level comparisons to run with sufficient data availability. This file  was compiled in the script 'Compile data for analysis.R' 

- comparison_jobs_groups.csv
List of Group-level comparisons to run with sufficient data availability. This file  was compiled in the script 'Compile data for analysis.R' 

- comparison_jobs_less_good.csv
List of Order-level comparisons without sufficient data availability. This file  was compiled in the script 'Compile data for analysis.R' 

- comparison_jobs_groups_less_good.csv
List of Group-level comparisons to run without sufficient data availability. This file  was compiled in the script 'Compile data for analysis.R' 









