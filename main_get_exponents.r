# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# selects the best model out of WG2 and MLR to create global gamma plots and run the model worldwide
rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)

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


# ============================================================================
# Selection of Basins
# ============================================================================

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge = "on")
red_x_orig <- x_orig[reducer, ]
red_y <- y[reducer, ]

thresholds <- get_classes_for_gamma(red_y$mean_gamma, n_centers = 3, print_plot = FALSE) [["dfThresholds"]]
lower_bound_tuning <- thresholds[2, 1]
upper_bound_tuning <- thresholds[1, 3]

columns_new_method <- c(2, 5, 6, 7, 10, 12, 15, 16, 17, 19, 22, 25)
colnames(x_orig)[columns_new_method]

columns_b2b <- c(2, 19, 5, 18, 10, 17)
names(x_orig[columns_b2b])



################################################################################
# getting coefficients
################################################################################

x=red_x_orig[,columns_b2b]
y=red_y$mean_gamma
data=x
data$gamma=y
b2b <- lm(log(data$gamma) ~ ., data)
gamma_b2b <- exp(predict(b2b, newdata=data))
gamma_b2b <- limit_gamma_to_be_valid(gamma_b2b,
                                     upper_limit=5.,
                                     lower_limit=0.1)

x=red_x_orig[,columns_new_method]
y=red_y$mean_gamma
data=x
data$gamma=y
mlr <- lm(data$gamma ~ ., data)
gamma_mlr <- exp(predict(mlr, newdata=data))
gamma_mlr <- limit_gamma_to_be_valid(gamma_mlr,
                                     upper_limit=upper_bound_tuning,
                                     lower_limit=lower_bound_tuning)


mlr_quality <- mae(obs=log(y+1), sim=log(gamma_mlr+1))
b2b_quality <- mae(obs=log(y+1), sim=log(gamma_b2b+1))

saveRDS(b2b, "./data/regression_b2b.rds")
saveRDS(mlr, "./data/regression_mlr.rds")

