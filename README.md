# Digital Supplement for Kupzig et al. (2024)

This repository is the supplement of Kupzig et al. (2024). It contains the underlying data to reproduce the results and the applied r-code to run the analysis and create plots for visualization. In the following, the main parts of the repository are described briefly.


## Code
The code is written in R and uses the packages
- randomForest
- class
- stats
- ggplot2
- tidyr
- dplyr
- raster

To run the analysis and re-create Figure 3 and 4 from the paper please download the repo to your local computer. Change line 1 in main_run.r to the location of the script/repository and run the files. This will produce a split sample test ensemble of 100 members and create three plots in plots-folder.

There are also several scripts added to create additional plots from the paper (all notes with main_*.r). In case you find a bug, do not hesitate to create an issue.

 *<< Software is written by humans and therefore has bugs.>>*
 John Jacobs

## Data
- **NEW_IDs.rds**: order of IDs in NEW_x_orig.rds and NEW_y.rds and NEW_distCentrodis.rds
- **NEW_distCentrodis.rds**: centroids-based distance for all 1861 catchments.
- **NEW_x_orig.rds**: catchment descriptors, order is same as in NEW_y.rds
- **NEW_y.rds**: result of calibration: ID - mean_CorrF - mean_gamma - sd_gamma
- **NEW_quality_monthly_bias.rds**: catchment quality: ID & bias (other order than NEW_IDs!)

- **monthly_kge_below_04.txt**: (NEU)
- **estimates.rds**: (NEU)

- **LocalData-Folder**: data to plot Figure 6 (VERÄNDERT)
- **global_gamma.tif**: stacked raster for the worldwide gamma distribution using the five selected methods (layer order: B2B, KNN 'p+cl', MLR* 'p+cl', ensemble SI* 'subset', SP)
- **ungauged_region.grd**: raster to distinguish between ungauged (1) and gauged (2) regions (note that gauged regions which are neglected are defined with -1 in this layer)  (VERÄNDERT)
- **global_smax.tiff**: (NEU)


## Plots
In this folder the plots will appear after installing all necessary dependencies (s. above) and running the main_*.r scripts. These plots won't be exactly the same, due to random effects. However, due to the applied set.seed() the plots will not change when running them multiple times.