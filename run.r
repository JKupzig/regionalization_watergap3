setwd(r"(C:/Users/jenny/MyProject_sciebo/_Nina/Regionalization/R/data_availability)")
# set you own working directory (position of run.r) here

rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)


source("./split_sample_test.r")
source("./plotting.r")
source("./helper_functions.r")


# ============================================================================
# Loading Variables (calculated in Prepare_Data at the cluster)
# ============================================================================

MIN_SIZE <- 5000
MIN_QUALITY <- 0.2
SAMPLING_NUMBER <- 100 #here 100 is used in paper

folder2use <- "./data"
TARGETFOLDER <- "./plots"

#Load Vars
x_orig <- readRDS(file.path(folder2use, "NEW_x_orig_wClimate.rds"))
y <- readRDS(file.path(folder2use, "NEW_y.rds"))

distShape <- readRDS(file.path(folder2use, "NEW_distCentroid.rds"))

# ============================================================================
# Selection of Basins
# ============================================================================

reducer <- create_subset(MIN_QUALITY, MIN_SIZE)
red_x_orig <- x_orig[reducer,]
red_y <-  y[reducer,]

red_distShape <- distShape[reducer,reducer]

#Functions to use gamma
df3_gamma <- getClasses4Gamma(red_y$mean_gamma, nCenters=3, printPlots=F) [["df"]]

thresholds <- getClasses4Gamma(red_y$mean_gamma, nCenters=3, printPlots=F) [["dfThresholds"]]
tune1 <- thresholds[2,1] #used in regression models
tune2 <- thresholds[1,3] #used in regression models

# ============================================================================
# Split Sample Test
# ============================================================================

cols_all = c(2, 5, 6, 7, 10, 12, 15, 16, 17, 19, 22, 25);  names(red_x_orig)[cols_all]
type_all <- split_sample_test(
  nrepeats=SAMPLING_NUMBER,
  column_idx=cols_all,
  tuning_pars=c(tune1, tune2),
  catchment_distances=red_distShape,
  catchment_characteristics=red_x_orig,
  catchment_gamma=df3_gamma$gamma)


cols_climatic=c(19, 25); names(red_x_orig)[cols_climatic]
type_climatic <- split_sample_test(
  nrepeats=SAMPLING_NUMBER,
  column_idx=cols_climatic,
  tuning_pars=c(tune1, tune2),
  catchment_distances=red_distShape,
  catchment_characteristics=red_x_orig,
  catchment_gamma=df3_gamma$gamma)


cols_physiographic=c(10, 16, 17); names(red_x_orig)[cols_physiographic]
type_physio <- split_sample_test(
  nrepeats=SAMPLING_NUMBER,
  column_idx=cols_physiographic,
  tuning_pars=c(tune1, tune2),
  catchment_distances=red_distShape,
  catchment_characteristics=red_x_orig,
  catchment_gamma=df3_gamma$gamma)


cols_physio_clima =c(cols_physiographic, cols_climatic); names(red_x_orig)[cols_physio_clima]
type_physio_climatic <- split_sample_test(
  nrepeats=SAMPLING_NUMBER,
  column_idx=cols_physio_clima,
  tuning_pars=c(tune1, tune2),
  catchment_distances=red_distShape,
  catchment_characteristics=red_x_orig,
  catchment_gamma=df3_gamma$gamma)


#Plots for evaluation
type_all$info <- rep("all", nrow(type_climatic))
type_climatic$info <- rep("cl", nrow(type_climatic))
type_physio$info <- rep("p", nrow(type_physio))
type_physio_climatic$info <- rep("p+cl", nrow(type_physio_climatic))


# ============================================================================
# Plotting Figure 3 and 4
# ============================================================================

physio_first_plot <- type_physio[names(type_physio) %in% c("cal_MLR", "val_MLR", "cal_SI_1", "val_SI_1", "info")]
climatic_first_plot <-  type_climatic[names(type_climatic) %in% c("cal_MLR", "val_MLR", "cal_SI_1", "val_SI_1", "info")]
physio_climatic_first_plot <- type_physio_climatic[names(type_physio_climatic) %in%
                                                    c("cal_MLR", "val_MLR",
                                                      "cal_MLR_t", "val_MLR_t",
                                                      "cal_SI_1", "val_SI_1",
                                                      "cal_SI_10", "val_SI_10",
                                                      "cal_SI_10_t", "val_SI_10_t",
                                                       "val_SP_1", 
                                                      "info")]
all_first_plot <- type_all[names(type_all) %in%
                             c("cal_MLR", "val_MLR",
                               "cal_SI_1", "val_SI_1",
                               "cal_WG2", "val_WG2",
                               "info")]

beneficial_information <- first_boxplots(climatic_first_plot, physio_first_plot,
                                         physio_climatic_first_plot, all_first_plot)


ggsave(file=file.path(TARGETFOLDER, "Figure_3.png"), beneficial_information,
       width = 25,
       height = 12,
       units = "cm",
       dpi = 300,)

################################################################################

physio_second_plot <- type_physio[names(type_physio) %in% c("cal_RF", "val_RF",
                                                            "cal_k-means", "val_k-means",
                                                            "info")]
climatic_second_plot <-  type_climatic[names(type_climatic) %in% c("cal_RF", "val_RF",
                                                                   "cal_k-means", "val_k-means",
                                                                   "info")]
physio_climatic_second_plot <- type_physio_climatic[names(type_physio_climatic) %in%
                                                     c("cal_RF", "val_RF",
                                                       "cal_RF_t", "val_RF_t",
                                                       "cal_k-means", "val_k-means",
                                                       "info")]
all_second_plot <- type_all[names(type_all) %in%
                              c("cal_RF", "val_RF",
                                "cal_RF_t", "val_RF_t",
                                "cal_k-means", "val_k-means",
                                "cal_k-means_all",  "val_k-means_all",
                                "cal_WG2", "val_WG2",
                                "info")]

ml_information <- first_boxplots(climatic_second_plot, physio_second_plot,
                                         physio_climatic_second_plot, all_second_plot)


ggsave(file=file.path(TARGETFOLDER, "Figure_4.png"), ml_information,
       width = 25,
       height = 12,
       units = "cm",
       dpi = 300,)

median_physio <- apply(type_physio[,-c(33)], 2, function(x) {median(x, na.rm=T)} )
sd_physio <- apply(type_physio[,-c(33)], 2, function(x) {sd(x, na.rm=T)} )

median_climate<- apply(type_climatic[,-c(33)], 2, function(x) {median(x, na.rm=T)} )
sd_climate <- apply(type_climatic[,-c(33)], 2, function(x) {sd(x, na.rm=T)} )

median_physio_climate <- apply(type_physio_climatic[,-c(33)], 2, function(x) {median(x, na.rm=T)} )
sd_physio_climate <- apply(type_physio_climatic[,-c(33)], 2, function(x) {sd(x, na.rm=T)} )

median_all <- apply(type_all[,-c(33)], 2, function(x) {median(x, na.rm=T)} )
sd_all <- apply(type_all[,-c(33)], 2, function(x) {sd(x, na.rm=T)} )

data2save_a <- rbind(median_physio, median_climate, median_physio_climate, median_all)
data2save_b <- rbind(sd_physio, sd_climate, sd_physio_climate, sd_all)
data2save <- cbind(data2save_a, data2save_b)
write.table(data2save, file.path("./data/appendix_2_raw.csv"), col.names = T, row.names = F, sep="\t")

