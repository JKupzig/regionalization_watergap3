# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)

source("./algorithm/apply.Beck2016.R")
source("./algorithm/apply.knn.R")
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
distance_matrix <- distance_to_centroid[reducer, reducer]

thresholds <- get_classes_for_gamma(red_y$mean_gamma, n_centers = 3, print_plot = FALSE) [["dfThresholds"]]
lower_bound_tuning <- thresholds[2, 1]
upper_bound_tuning <- thresholds[1, 3]

columns_p_cl <- c(2, 5, 6, 7, 10, 12, 15, 16, 17, 19, 22, 25)
columns_subset = c(10, 16, 17, 19, 25)

################################################################################
# getting coefficients
################################################################################

set.seed(123)
ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.99, 0.01))

sp <- run.spatialProximity(
  ind=ind,
  distances=distance_matrix,
  gamma=red_y$mean_gamma,
  k=1)

sp_robust <- run.spatialProximity(
  ind=ind,
  distances=distance_matrix,
  gamma=red_y$mean_gamma,
  k=1,
  robustness_in_train = T)

gamma_train <- sp$calibrated_gamma[sp$mapping==1]
sp_y_est <- sp$predicted_gamma[sp$mapping==1]
sp_y_est_robust <- sp_robust$predicted_gamma[sp$mapping==1]
par(mfrow=c(1,2))
boxplot(list("normal estimates"=sp_y_est-gamma_train,
             "excluding nearest basin"=sp_y_est_robust-gamma_train),
        main="SP", ylab=" gamma (regionalized) - gamma (calibrated)")
plot(sp_y_est-sp_y_est_robust, main="SP",
     xlab="basins", ylab="difference in estimates")


knn <- run.knn(
  ind=ind,
  x=red_x_orig[columns_p_cl],
  y=red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning)
)

knn_robust <- run.knn(
  ind=ind,
  x=red_x_orig[columns_p_cl],
  y=red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning),
  robustness_in_train = T
)

knn_y_est <- knn$gamma_cal_limited
knn_y_est_robust <- knn_robust$gamma_cal_limited
boxplot(list("normal estimates"=knn_y_est-gamma_train,
             "excluding nearest basin"=knn_y_est_robust-gamma_train),
             main="KNN", ylab=" gamma (regionalized) - gamma (calibrated)")
plot(knn_y_est-knn_y_est_robust, main="KNN",
     xlab="basins", ylab="difference in estimates")

SI <- run.Beck2016(
  ind=ind,
  x=red_x_orig[columns_subset],
  y=red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning),
  n_ensemble=1
)

SI_robust <- run.Beck2016(
  ind=ind,
  x=red_x_orig[columns_subset],
  y=red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning),
  n_ensemble=1,
  robustness_in_train = T
)

si_y_est <- SI$gamma_cal_limited
si_y_est_robust <- SI_robust$gamma_cal_limited

boxplot(list("normal estimates"=si_y_est-gamma_train,
             "excluding nearest basin"=si_y_est_robust-gamma_train),
        main="SI", ylab=" gamma (regionalized) - gamma (calibrated)")
plot(si_y_est-si_y_est_robust, main="SI",
     xlab="basins", ylab="difference in estimates")


