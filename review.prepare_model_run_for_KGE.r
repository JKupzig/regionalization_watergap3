# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# selects the best model out of WG2 and MLR to create global gamma plots and run the model worldwide
rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)

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

reducer <- create_subset(MIN_QUALITY, MIN_SIZE)
red_x_orig <- x_orig[reducer, ]
red_y <- y[reducer, ]
distance_matrix <- distance_to_centroid[reducer, reducer]

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

set.seed(123)
ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.50, 0.50))

mlr <- apply.MultipleLinearRegression(
  ind, red_x_orig[columns_new_method], red_y$mean_gamma,
  mod = TRUE, tuningPars = c(lower_bound_tuning, upper_bound_tuning),
  return_regression = TRUE)

mlr_y_est <- new_model(red_x_orig[ind==2, columns_new_method],
                   mlr$regression$coefficients,
                   lower_bound_tuning, upper_bound_tuning)

b2b <-  apply.MultipleLinearRegression(
  ind,
  red_x_orig[columns_b2b],
  red_y$mean_gamma,
  ln = "on", return_regression = TRUE)

b2b_y_est <- wg2_model(red_x_orig[ind==2, columns_b2b], b2b$regression$coefficients)

sp <- apply.spatialProximity(
  ind,
  distance_matrix,
  red_y$mean_gamma)
sp_y_est <- sp$result$y_estimated[ind==2]

boxplot(list("mlr"=mlr_y_est,
             "b2b"=b2b_y_est,
             "sp"=sp_y_est,
             "calibrated"=sp$result$y_given[ind==2]))

################################################################################
# creating spatial gamma information
################################################################################

for (cont in c("af", "as", "au", "eu", "na", "sa"))
{
  basins_matrix <- watergap3data::unf.readunf(sprintf("./data/grdc_spatial/G_BASCALIB_GRDC_%s.UNF0", cont), cont)
  basins4regression <- red_y[ind==2, 1]
  basins4calibration <- red_y[ind==1, 1]

  basins_matrix_sp <- basins_matrix
  basins_matrix_mlr <- basins_matrix
  basins_matrix_b2b <- basins_matrix

  filename_sp <- sprintf("./data/grdc_spatial/G_GAMMA_HBV_SP_%s.UNF0", cont)
  filename_mlr <- sprintf("./data/grdc_spatial/G_GAMMA_HBV_MLR_%s.UNF0", cont)
  filename_b2b <- sprintf("./data/grdc_spatial/G_GAMMA_HBV_B2B_%s.UNF0", cont)

  count <- 1
  for (basin in basins4regression)
  {
    basins_matrix_sp[basins_matrix_sp==basin & !is.na(basins_matrix_sp)] <- sp_y_est[count]
    basins_matrix_mlr[basins_matrix_mlr==basin & !is.na(basins_matrix_sp)] <- mlr_y_est[count]
    basins_matrix_b2b[basins_matrix_b2b==basin & !is.na(basins_matrix_sp)] <- b2b_y_est[count]

    count <- count + 1
  }

  count <- 1
  for (basin in basins4calibration)
  {
    basins_matrix_sp[basins_matrix_sp==basin & !is.na(basins_matrix_sp)] <- sp$result$y_given[ind==1][count]
    basins_matrix_mlr[basins_matrix_mlr==basin & !is.na(basins_matrix_sp)] <- sp$result$y_given[ind==1][count]
    basins_matrix_b2b[basins_matrix_b2b==basin & !is.na(basins_matrix_sp)] <- sp$result$y_given[ind==1][count]

    count <- count + 1
  }


  basins_matrix_sp[basins_matrix_sp > 5 ] <- 2.5
  basins_matrix_mlr[basins_matrix_mlr > 5] <- 2.5
  basins_matrix_b2b[basins_matrix_b2b > 5] <- 2.5

  #basins_matrix_sp[is.na(basins_matrix_sp)] <- 5.1
  #basins_matrix_mlr[is.na(basins_matrix_mlr)] <- 5.1
  #basins_matrix_b2b[is.na(basins_matrix_b2b)] <- 5.1

  writeBin(as.numeric(basins_matrix_sp), filename_sp, size=4, endian="big")
  writeBin(as.numeric(basins_matrix_mlr), filename_mlr, size=4, endian="big")
  writeBin(as.numeric(basins_matrix_b2b), filename_b2b, size=4, endian="big")

  raster::plot(watergap3data::unf.vec2raster(basins_matrix_sp, cont=cont), main="SP") # to look at data!
  raster::plot(watergap3data::unf.vec2raster(basins_matrix_mlr, cont=cont), main="MLR") # to look at data!
  raster::plot(watergap3data::unf.vec2raster(basins_matrix_b2b, cont=cont),main="B2B") # to look at data!
}


