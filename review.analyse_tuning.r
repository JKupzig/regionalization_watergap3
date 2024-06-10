# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# selects the best model out of WG2 and MLR to create global gamma plots and run the model worldwide
rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)

source("./algorithm/apply.Beck2016.R")
source("./algorithm/apply.knn.R")
source("./algorithm/apply.randomForest.R")
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
distance_matrix <- distance_to_centroid[reducer, reducer]

thresholds <- get_classes_for_gamma(red_y$mean_gamma, n_centers = 3, print_plot = FALSE) [["dfThresholds"]]
lower_bound_tuning <- thresholds[2, 1]
upper_bound_tuning <- thresholds[1, 3]

columns_p <- c(2, 5, 6, 7, 10, 12, 15, 16, 17)
columns_p_cl <- c(2, 5, 6, 7, 10, 12, 15, 16, 17, 19, 22, 25)
columns_cl <- c(19,22,25)
columns_subset <- c(10, 16, 17, 19, 25)
columns_b2b <- c(2, 19, 5, 18, 10, 17)

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

idx = 48
predictions_result <- array(NA, dim=c(7, sum(ind_list[[idx]] == 2), 2))


mlr <- run.MultipleLinearRegression(
  ind_list[[idx]],
  red_x_orig[columns_p],
  red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning),
  ln="off"
)
predictions_result[1,,1] <- mlr$gamma_val_limited
predictions_result[1,,2] <- mlr$gamma_val_tuned


mlr_b2b <- run.MultipleLinearRegression(
  ind_list[[idx]],
  red_x_orig[columns_b2b],
  red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning),
  ln="on"
)
predictions_result[2,,1] <- mlr_b2b$gamma_val_limited
predictions_result[2,,2] <- mlr_b2b$gamma_val_tuned


sp <- run.spatialProximity(
  ind = ind_list[[idx]],
  distances=distance_matrix,
  gamma=red_y$mean_gamma,
  k=1)
gamma_val <- sp$predicted_gamma[sp$mapping==2]

predictions_result[3,,1] <- gamma_val
predictions_result[3,,2] <- limit_gamma_to_be_valid(gamma_val,
                                                    upper_limit=upper_bound_tuning,
                                                    lower_limit=lower_bound_tuning)


knn <- run.knn(
  ind = ind_list[[idx]],
  x=red_x_orig[columns_p],
  y=red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning)
)
predictions_result[4,,1] <- knn$gamma_val_limited
predictions_result[4,,2] <- knn$gamma_val_tuned


SI_10 <- run.Beck2016(
  ind = ind_list[[idx]],
  x=red_x_orig[columns_p],
  y=red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning),
  n_ensemble=10
)
predictions_result[5,,1] <- SI_10$gamma_val_limited
predictions_result[5,,2] <- SI_10$gamma_val_tuned


SI_1 <- run.Beck2016(
  ind = ind_list[[idx]],
  x=red_x_orig[columns_p],
  y=red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning),
  n_ensemble=1
)
predictions_result[6,,1] <- SI_1$gamma_val_limited
predictions_result[6,,2] <- SI_1$gamma_val_tuned

RF <- run.RandomForest(
  ind = ind_list[[idx]],
  x=red_x_orig[columns_p],
  y=red_y$mean_gamma,
  tuningPars = c(lower_bound_tuning, upper_bound_tuning),
  NTrees=200,
  mTRY=2
)
predictions_result[7,,1] <- RF$gamma_val_limited
predictions_result[7,,2] <- RF$gamma_val_tuned

sp_idx <- 3
mlr_idx <- 1
b2b_idx <- 2
idx_knn <- 4
si1_idx <- 6
si10_idx <- 7

to_rgb <- function(red, green, blue, alpha){
  MAX_VALUE <- 255
  return(rgb(red/MAX_VALUE, green/MAX_VALUE, blue/MAX_VALUE, alpha))
}

idx_to_plot <- sp_idx
df <- data.frame(standard=predictions_result[idx_to_plot,,1],
                 tuned=predictions_result[idx_to_plot,,2],
                 calibrated=RF$gamma_val_calibrated)

df_long <- tidyr::pivot_longer(df,cols=everything())
df_long$name <- factor(df_long$name, levels=c("calibrated", "tuned", "standard"))
ggplot(df_long, aes(x=value, col=name)) +
  stat_ecdf(geom="point") +
  stat_ecdf(geom="step") +
  scale_color_manual(name="method",
                     values=c(to_rgb(0, 119/255, 187, 0.8),
                              to_rgb(51, 187, 238, 0.8),
                              to_rgb(0, 153, 136, 0.8))) +
  theme_bw() +
  theme(
       legend.position=c(.8,.25)
  ) +
  xlab("Gamma")

ggsave()

