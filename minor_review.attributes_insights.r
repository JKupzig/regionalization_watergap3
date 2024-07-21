# selects the best model out of WG2 and MLR to create global gamma plots and run the model worldwide
rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)

source("./algorithm/apply.knn.R")
source("./algorithm/apply.MultipleLinearRegression.R")
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

# ============================================================================
# Understanding multi-collinearity
# ============================================================================

columns_p_cl <- c(2, 5, 6, 7, 10, 12, 15, 16, 17, 19, 22, 25)
colnames(x_orig)[columns_p_cl]

columns_p_cl_no_multicol <- c(2, 5, 7, 10, 12, 15, 16, 17, 22, 25)
colnames(x_orig)[columns_p_cl_no_multicol]

# variance inflation factor: > 10 = multicolinearity
vif_all <- run.variance_inflation_factor(
  red_x_orig[columns_p_cl],
  red_y$mean_gamma,
)

vif_all_reduced <- run.variance_inflation_factor(
  red_x_orig[columns_p_cl_no_multicol],
  red_y$mean_gamma,
)

print(vif_all)
print(vif_all_reduced)

# ============================================================================
# creating importance bars using manually permutation (MLR)
# ===========================================================================
labels2use <- c("mean_smax" = "Soil Storage",
                "mean_op_water" = "Open Water Bodies",
                "mean_wetland" = "Wetlands",
                "areaBasin" = "Size",
                "mean_slope" = "*Slope",
                "mean_altitude" = "Altitude",
                "mean_sealedArea" = "Sealed Area",
                "mean_Forest" = "*Forest",
                "mean_permaglac" = "*Permafrost & Glacier",
                "mean_temp" = "*Mean Temperature",
                "sum_prec" = "Yearly Precipitation",
                "sum_sw" = "*Yearly Shortwave Downward Radiation",
                "gamma" = "Calibration Parameter")

new_names <- c()
for (var_name in names(red_x_orig[,columns_p_cl]))
{
  new_names <- c(new_names, labels2use[[var_name]])
}

apply_mlr_model <- function(regr, x, y, upper, lower){
  gamma_cal <- predict(regr, newdata=x)
  gamma_cal_tuned <- limit_gamma_to_be_valid(gamma_cal,
                                             upper_limit=upper,
                                             lower_limit=lower)
  benchmark <- logmae(obs=y,
                      sim=gamma_cal_tuned)
  return(benchmark)
}

set.seed(123)
N_SAMPLES <- 100

ind_list <- list()
for (i in 1:N_SAMPLES){
  ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.50, 0.50))
  ind_list[[i]] <- ind
}

idx = 48

test_x <- red_x_orig[,columns_p_cl][ind_list[[idx]]==2,]
test_y <- red_y$mean_gamma[ind_list[[idx]]==2]

original_model <- run.MultipleLinearRegression(
  ind_list[[idx]],
  red_x_orig[,columns_p_cl],
  red_y$mean_gamma,
  tuningPars=c(lower_bound_tuning,upper_bound_tuning),
  ln="off",
  return_regression = TRUE)


original_benchmark <- apply_mlr_model(original_model,
                                  test_x,
                                  test_y,
                                  upper_bound_tuning,
                                  lower_bound_tuning)

shuffled_benchmarks <- c()
for (column in columns_p_cl){
  shuffled_data <- red_x_orig
  shuffled_data[,column] <- sample(shuffled_data[,column])
  test_x <- shuffled_data[,columns_p_cl][ind_list[[idx]]==2,]
  shuffled_model <- apply_mlr_model(original_model,
                                    test_x,
                                    test_y,
                                    upper_bound_tuning,
                                    lower_bound_tuning)
  shuffled_benchmarks <- c(shuffled_benchmarks, shuffled_model)
}



png("./plots/importance_mlr.png", res=300, width=16, height=12, units="cm")
par(mar=c(9.2,5,4,1))
ylab = expression(paste(Delta,"logMAE shuffling predictor (%)"))
barplot((shuffled_benchmarks-original_benchmark) / original_benchmark * 100,
        names.arg=new_names,
        las=2, ylab=ylab)
dev.off()
# ============================================================================
# creating importance bars using manually permutation (KNN)
# ===========================================================================
set.seed(123)
prediction_benchmark_model <- run.knn(
    ind_list[[idx]],
    red_x_orig[,columns_p_cl],
    red_y$mean_gamma,
    tuningPars=c(lower_bound_tuning, upper_bound_tuning),
    robustness_in_train=FALSE,
    shuffle=NULL)

benchmark <- logmae(obs=prediction_benchmark_model$gamma_val_calibrated,
                    sim=prediction_benchmark_model$gamma_val_limited)

shuffled_benchmarks_knn <- c()
count <- 1
for (column in columns_p_cl){
  prediction_shuffled_model <- run.knn(
    ind_list[[idx]],
    red_x_orig[,columns_p_cl],
    red_y$mean_gamma,
    tuningPars=c(lower_bound_tuning, upper_bound_tuning),
    robustness_in_train=FALSE,
    shuffle=count)
  count <- count + 1

  shuffled_benchmark <- logmae(
    obs=prediction_shuffled_model$gamma_val_calibrated,
    sim=prediction_shuffled_model$gamma_val_limited)

  shuffled_benchmarks_knn <- c(shuffled_benchmarks_knn, shuffled_benchmark)
}

png("./plots/importance_knn.png", res=300, width=16, height=12, units="cm")
par(mar=c(9.2,5,4,1))
ylab = expression(paste(Delta,"logMAE shuffling predictor (%)"))
barplot((shuffled_benchmarks_knn-benchmark) / benchmark * 100,
        names.arg=new_names,
        las=2, ylab=ylab)
abline(h=10, col="lightgrey", lty="dashed", lwd=2)
dev.off()
