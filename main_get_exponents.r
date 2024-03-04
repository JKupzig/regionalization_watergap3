#setwd(r"(C:\Users\jenny\MyProject_sciebo\_Nina\Regionalization\R\data_availability\regionalization_watergap3)")
# set you own working directory (position of run.r) here

# selects the best model out of WG2 and MLR to create global gamma plots and run the model worldwide
rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)

source("./algorithm/apply.MultipleLinearRegression.R")
source("./helper_functions.r")

# ============================================================================
# Prepare Logging
# ============================================================================
my_logfile <- "results_exponents.txt"
if (file.exists(my_logfile)) { file.remove(my_logfile) }
my_console_appender = log4r::console_appender(layout = log4r::default_log_layout())
my_file_appender <- log4r::file_appender(my_logfile,
                                 append = TRUE,
                                 layout = log4r::default_log_layout())
my_logger <- log4r::logger(threshold = "INFO",
                           appenders = list(my_console_appender,my_file_appender))

# ============================================================================
# Loading Variables
# ============================================================================
MIN_SIZE <- 5000
MIN_QUALITY <- 0.2
SAMPLING_NUMBER <- 100

log4r::info(my_logger, sprintf("settings: min_size=%g, min_quality=%f, n_samples=%g",
                               MIN_SIZE, MIN_QUALITY, SAMPLING_NUMBER))

#Load Vars
folder2use <- "./data"
x_orig <- readRDS(file.path(folder2use, "NEW_x_orig.rds"))
y <- readRDS(file.path(folder2use, "NEW_y.rds"))


# ============================================================================
# Selection of Basins
# ============================================================================

reducer <- create_subset(MIN_QUALITY, MIN_SIZE)
red_x_orig <- x_orig[reducer,]
red_y <- y[reducer,]

thresholds <- get_classes_for_gamma(red_y$mean_gamma, n_centers = 3, print_plot = FALSE) [["dfThresholds"]]
lower_bound_tuning <- thresholds[2, 1]
upper_bound_tuning <- thresholds[1, 3]

columns_new_method <- c(10, 16, 17, 19, 25)
colnames(x_orig)[columns_new_method]
columns_b2b <- c(2, 19, 5, 18, 10, 17)
names(x_orig[columns_b2b])
columns_false <- c(19,25)
names(x_orig[columns_false])


################################################################################
# getting coefficients
################################################################################
coefficients_new <- NULL; maes <- NULL
pb <- txtProgressBar(min = 1, max = SAMPLING_NUMBER, style = 3)
set.seed(123)

for (i in 1:SAMPLING_NUMBER) {
  setTxtProgressBar(pb, i)
  ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.50, 0.50))
  maes_mlr <- apply.MultipleLinearRegression(
    ind, red_x_orig[columns_new_method], red_y$mean_gamma,
    mod = TRUE, tuningPars = c(lower_bound_tuning, upper_bound_tuning),
    return_regression = TRUE)

  y_est <- new_model(red_x_orig[columns_new_method],
                     maes_mlr$regression$coefficients,
                     lower_bound_tuning, upper_bound_tuning)

  bm <- mean(abs(y_est - red_y$mean_gamma))
  coefficients_new <- rbind(coefficients_new, maes_mlr$regression$coefficients)
  maes <- c(maes, bm)
}

idx <- which(maes == min(maes))
coeffs_new <- coefficients_new[idx, ]
log4r::info(my_logger, "#####################################")
log4r::info(my_logger, "Info to use for MLR approach:")
log4r::info(my_logger, sprintf("Tuning pars: %f & %f", lower_bound_tuning, upper_bound_tuning))
for (name in names(coeffs_new)) {
  log4r::info(my_logger, sprintf("%s: %f", name, coeffs_new[name]))
}


set.seed(123)
coefficients_b2b <- NULL; b2b_maes <- NULL
for (i in 1:SAMPLING_NUMBER){
  setTxtProgressBar(pb, i)
  ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.50, 0.50))
  maes_b2b <-  apply.MultipleLinearRegression(
    ind,
    red_x_orig[columns_b2b],
    red_y$mean_gamma,
    ln = "on", return_regression = TRUE)

  y_est <- wg2_model(red_x_orig[columns_b2b], maes_b2b$regression$coefficients)
  quality <- mean(abs(y_est - red_y$mean_gamma))
  b2b_maes <- c(b2b_maes, quality)
  coefficients_b2b <- rbind(coefficients_b2b, maes_b2b$regression$coefficients)
}

idx <- which(b2b_maes == min(b2b_maes))
coeffs_wg2 <- coefficients_b2b[idx,]
log4r::info(my_logger, "#####################################")
log4r::info(my_logger, "Info to use for WG2 approach:")
for (name in names(coeffs_wg2)) {
  log4r::info(my_logger, sprintf("%s: %f", name, coeffs_wg2[name]))
}


######################################

# evaluating model for all gauged basins
y_est_new <- new_model(
  red_x_orig[columns_new_method],
  coeffs_new,
  lower_bound_tuning, upper_bound_tuning)

y_est_wg2 <- wg2_model(
  red_x_orig[columns_b2b],
  coeffs_wg2)

mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

log(median(y_est_wg2))
median(y_est_new)

most_simple_benchmark <- mean(abs(mean(red_y$mean_gamma) - red_y$mean_gamma))
wg2_benchmark <- mean(abs(y_est_wg2 - red_y$mean_gamma))
mlr_benchmark <- mean(abs(y_est_new - red_y$mean_gamma))
log4r::info(my_logger, "#####################################")
log4r::info(my_logger, sprintf("MAE when using mean as predictor: %f", most_simple_benchmark))
log4r::info(my_logger, sprintf("MAE when using WG2 as predictor: %f", wg2_benchmark))
log4r::info(my_logger, sprintf("MAE when using new MLR as predictor: %f", mlr_benchmark))

