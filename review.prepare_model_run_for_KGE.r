# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# selects the best model out of WG2 and MLR to create global gamma plots and run the model worldwide
rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)

source("./algorithm/apply.Beck2016.R")
source("./algorithm/apply.knn.R")
source("./algorithm/apply.kmeans.R")
source("./algorithm/apply.MultipleLinearRegression.R")
source("./algorithm/apply.spatialProximity.R")
source("./helper_functions.r")

# ============================================================================
# Loading Variables
# ============================================================================

MIN_SIZE <- 5000
MIN_QUALITY <- 0.2

#Load Vars
ROOT <- "./data"
x_orig <- readRDS(file.path(ROOT, "NEW_x_orig.rds"))
y <- readRDS(file.path(ROOT, "NEW_y.rds"))
distance_to_centroid <- readRDS(file.path(ROOT, "NEW_distCentroid.rds"))


# ============================================================================
# Selection of Basins
# ============================================================================

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge = "on")
red_x_orig <- x_orig[reducer, ]
red_y <- y[reducer, ]

# valid_basins <- as.data.frame(list("ids"=red_y$ID))
# write.table(valid_basins, "./data/valid_basins.txt", row.names=F, col.names=T)

distance_matrix <- distance_to_centroid[reducer, reducer]

thresholds <- get_classes_for_gamma(red_y$mean_gamma, n_centers = 3, print_plot = FALSE) [["dfThresholds"]]
lower_bound_tuning <- thresholds[2, 1]
upper_bound_tuning <- thresholds[1, 3]

columns_p_cl <- c(2, 5, 6, 7, 10, 12, 15, 16, 17, 19, 22, 25)
colnames(x_orig)[columns_p_cl]

columns_cl <- c(19,22,25)
names(x_orig[columns_cl])

columns_subset <- c(10, 16, 17, 19, 25)
colnames(x_orig)[columns_subset]

columns_b2b <- c(2, 19, 5, 18, 10, 17)
names(x_orig[columns_b2b])

################################################################################
# getting coefficients
################################################################################

set.seed(123)
N_SAMPLES <- 100

ind_list <- list()
for (i in 1:N_SAMPLES){
  ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.50, 0.50))
  ind_list[[i]] <- ind
}

mae_result <- matrix(NA, nrow=10, ncol=N_SAMPLES)
#for (idx in 1:N_SAMPLES){
idx = 48

  mlr <- run.MultipleLinearRegression(
    ind_list[[idx]],
    red_x_orig[columns_p_cl],
    red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning),
    ln="off"
  )
  mlr_y_est <- mlr$gamma_val_tuned
  mae_result[1,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=mlr_y_est)

  mlr_bad <- run.MultipleLinearRegression(
    ind_list[[idx]],
    red_x_orig[columns_cl],
    red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning),
    ln="off"
  )
  mlr_bad_y_est <- mlr_bad$gamma_val_limited
  mae_result[2,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=mlr_bad_y_est)

  b2b <-  run.MultipleLinearRegression(
    ind_list[[idx]],
    red_x_orig[columns_b2b],
    red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning),
    ln="on"
  )
  b2b_y_est <- b2b$gamma_val_limited
  mae_result[3,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=b2b_y_est)


  sp <- run.spatialProximity(
    ind = ind_list[[idx]],
    distances=distance_matrix,
    gamma=red_y$mean_gamma,
    k=1)
  sp_y_est <- sp$predicted_gamma[sp$mapping==2]
  mae_result[4,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=sp_y_est)


  knn <- run.knn(
    ind = ind_list[[idx]],
    x=red_x_orig[columns_p_cl],
    y=red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning)
  )
  knn_y_est <- knn$gamma_val_limited
  mae_result[5,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=knn_y_est)

  knn_bad <- run.knn(
    ind = ind_list[[idx]],
    x=red_x_orig[columns_cl],
    y=red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning)
  )
  knn_bad_y_est <- knn_bad$gamma_val_tuned
  mae_result[6,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=knn_bad_y_est)

  SI <- run.Beck2016(
    ind = ind_list[[idx]],
    x=red_x_orig[columns_subset],
    y=red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning),
    n_ensemble=10
  )
  si_y_est <- SI$gamma_val_tuned
  mae_result[7,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=si_y_est)

  SI_bad <- run.Beck2016(
    ind = ind_list[[idx]],
    x=red_x_orig[columns_cl],
    y=red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning),
    n_ensemble=1
  )
  si_bad_y_est <- SI_bad$gamma_val_tuned
  mae_result[8,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=si_bad_y_est)


  kmeans_good <- run.kmeans(
    ind = ind_list[[idx]],
    x=red_x_orig[columns_subset],
    y=red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning)
  )
  kmeans_good_y_est <- kmeans_good$gamma_val_tuned
  mae_result[9,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=kmeans_good_y_est)
  kmeans_bad <- run.kmeans(
    ind = ind_list[[idx]],
    x=red_x_orig[columns_cl],
    y=red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning)
  )
  kmeans_bad_y_est <- kmeans_bad$gamma_val_limited
  mae_result[10,  idx] <- mae(obs=red_y$mean_gamma[ind_list[[idx]]==2],
                             sim=kmeans_bad_y_est)
# }
#
# merken <- list()
# for (i in 1:10){
#   lower_acceptance <- quantile(mae_result[i,], 0.25)
#   upper_acceptance <- quantile(mae_result[i,], 0.75)
#   ids <- which(mae_result[i,] >= lower_acceptance &
#                 mae_result[i,] <= upper_acceptance)
#   merken[[i]] <- ids
# }
#
# table(unlist(merken)) #id = 48
#
# boxplot(t(mae_result))
# points(mae_result[,48], col="firebrick", pch=15)

estimates<- data.frame(list(
 "mlr (good)"=mlr_y_est,
 "mlr (bad)"=mlr_bad_y_est,
 "b2b"=b2b_y_est,
 "sp"=sp_y_est,
 "knn (good)"=knn_y_est,
 "knn (bad)"=knn_bad_y_est,
 "si (good)"=si_y_est,
 "si (bad)"=si_bad_y_est,
 "kmeans (good)" = kmeans_good_y_est,
 "kmeans (bad)" = kmeans_bad_y_est,
 "calibrated"=red_y$mean_gamma[ind_list[[idx]]==2],
 "basin_id"=red_y$ID[ind_list[[idx]]==2]
 ))

saveRDS(estimates, "./data/estimates.rds")


################################################################################
# creating spatial gamma information
################################################################################


idx_overview <- list("1"=list(idx, sprintf("SP%i", idx), sp_y_est),
                     "2"=list(idx, sprintf("KNNGOOD%i", idx), knn_y_est),
                     "3"=list(idx, sprintf("KNNBAD%i", idx), knn_bad_y_est),
                     "4"=list(idx, sprintf("MLRGOOD%i", idx), mlr_y_est),
                     "5"=list(idx, sprintf("MLRBAD%i", idx), mlr_bad_y_est),
                     "6"=list(idx, sprintf("KMEANSGOOD%i", idx), kmeans_good_y_est),
                     "7"=list(idx, sprintf("KMEANSBAD%i", idx), kmeans_bad_y_est),
                     "8"=list(idx, sprintf("SIBAD%i", idx), si_bad_y_est),
                     "9"=list(idx, sprintf("SIGOOD%i", idx), si_y_est),
                     "10"=list(idx, sprintf("B2B%i", idx), b2b_y_est))


for (name in names(idx_overview)){
  print(name)
  for (cont in c("af", "as", "au", "eu", "na", "sa"))
  {
    print(cont)
    ind_id = idx_overview[[name]][[1]]
    ind <- ind_list[[ind_id]]
    estimated_gamma_in_psuedoungauged <- idx_overview[[name]][[3]]
    name2save <- idx_overview[[name]][[2]]

    filename_gamma <- sprintf("./data/grdc_spatial/G_GAMMA_HBV_%s_%s.UNF0", name2save, cont)
    basins_matrix <- watergap3data::unf.readunf(sprintf("./data/grdc_spatial/G_BASCALIB_GRDC_%s.UNF0", cont), cont)
    basins4regression <- red_y[ind==2, 1]
    basins4calibration <- red_y[ind==1, 1]

    count <- 1
    for (basin in basins4regression)
    {
      basins_matrix[basins_matrix==basin & !is.na(basins_matrix)] <- estimated_gamma_in_psuedoungauged[count]
      count <- count + 1
    }

    for (basin in basins4calibration)
    {
      gamma_to_assign <- red_y$mean_gamma[red_y$ID == basin]
      basins_matrix[basins_matrix==basin & !is.na(basins_matrix)] <- gamma_to_assign
    }

    for (basin in y$ID[!reducer]){
      gamma_to_assign <- y$mean_gamma[y$ID == basin]
      basins_matrix[basins_matrix==basin & !is.na(basins_matrix)] <- gamma_to_assign
    }

    basins_matrix[basins_matrix > 5 & !is.na(basins_matrix)] <- 2.5

    writeBin(as.numeric(basins_matrix), filename_gamma, size=4, endian="big")
  }
}


