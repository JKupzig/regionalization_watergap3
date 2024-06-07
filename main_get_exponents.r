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
coefficients_new <- NULL
maes <- NULL
pb <- txtProgressBar(min = 1, max = SAMPLING_NUMBER, style = 3)
set.seed(123)

regression_to_save_mlr <- list()
for (i in 1:SAMPLING_NUMBER) {
  setTxtProgressBar(pb, i)
  ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.50, 0.50))


  regression <- run.MultipleLinearRegression(
    ind,
    red_x_orig[columns_new_method],
    red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning),
    ln="off",
    return_regression = TRUE
  )

  regression_to_save_mlr[[i]] <- regression

  y_est <- predict(regression, red_x_orig[columns_new_method])
  y_est_tuned <- limit_gamma_to_be_valid(y_est, upper=upper_bound_tuning,
                          lower = lower_bound_tuning)
  bm <- mean(abs(y_est_tuned - red_y$mean_gamma))
  coefficients_new <- rbind(coefficients_new, regression$coefficients)
  maes <- c(maes, bm)
}

idx <- which(maes == min(maes))
saveRDS(regression_to_save_mlr[[idx]], "./data/regression_mlr.rds")

coeffs_new <- coefficients_new[idx, ]
log4r::info(my_logger, "#####################################")
log4r::info(my_logger, "Info to use for MLR approach:")
log4r::info(my_logger, sprintf("Tuning pars: %f & %f", lower_bound_tuning, upper_bound_tuning))
for (name in names(coeffs_new)) {
  log4r::info(my_logger, sprintf("%s: %f", name, coeffs_new[name]))
}


set.seed(123)
coefficients_b2b <- NULL; b2b_maes <- NULL
regression_to_save_b2b <- list()
for (i in 1:SAMPLING_NUMBER){
  setTxtProgressBar(pb, i)
  ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.50, 0.50))

  regression <- run.MultipleLinearRegression(
    ind,
    red_x_orig[columns_b2b],
    red_y$mean_gamma,
    tuningPars = c(lower_bound_tuning, upper_bound_tuning),
    ln="on",
    return_regression = TRUE
  )

  regression_to_save_b2b[[i]] <- regression
  y_est <- predict(regression, red_x_orig[columns_b2b])

  y_est_limited <- limit_gamma_to_be_valid(exp(y_est),
                                           upper=5,
                                           lower = 0.1)

  bm <- mean(abs(y_est_limited - red_y$mean_gamma))
  b2b_maes <- c(b2b_maes, bm)
  coefficients_b2b <- rbind(coefficients_b2b, regression$coefficients)
}

idx <- which(b2b_maes == min(b2b_maes))
coeffs_wg2 <- coefficients_b2b[idx,]
saveRDS(regression_to_save_b2b[[idx]], "./data/regression_b2b.rds")

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
  red_x_orig[columns_b2b],-
  coeffs_wg2)

