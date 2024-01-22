# Digital Supplement for Kupzig et al. (2024)

## Data
- NEW_IDs.rds - order of IDs in NEW_x_orig.rds and NEW_y.rds and NEW_distCentrodis.rds
- NEW_distCentrodis.rds - centroids-based distance for all 1861 catchments.
- NEW_x_orig.rds - catchment descriptors, order is same as in NEW_y.rds
- NEW_y.rds - result of calibration: ID - mean_CorrF - mean_gamma - sd_gamma
- NEW_quality_monthly_bias.rds - catchment quality: ID & bias (other order than NEW_IDs!)

## Code
The code is written in R and uses the packages
- randomForest
- class
- stats (base R)
- ggplot2
- tidyr
- dplyr

To run the analysis and re-create Figure 3 and 4 from the paper
please download the repo to your local computer. Change line 1 in
run.r to the location of the script/repository and run the files.
This will produce a split sample test ensemble of 10 members
(in paper 100 are used) and create to plots in plots-folder:
Figure_3.png and Figure_4.png.

In case for bug, do not hesitate to create an issue.

 << Software is written by humans and therefore has bugs.>>
 John Jacobs


## Plots
In this folder plots will appear after installing all
necessary dependencies (s. above) and running the main_*.r scripts. These plots
won't be exactly the same, due to random effects. However, due to
the applied set.seed() the plots will not change when running them
multiple times.