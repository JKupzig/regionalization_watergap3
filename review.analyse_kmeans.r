# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# analyses number of clusters for kmeans

rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)

source("./algorithm/apply.knn.R")
source("./algorithm/apply.kmeans.R")
source("./helper_functions.r")


# ============================================================================
# Loading Variables (calculated in Prepare_Data at the cluster)
# ============================================================================

MIN_SIZE <- 5000
MIN_QUALITY <- 0.2
SAMPLING_NUMBER <- 2 #here 100 is used in paper

folder2use <- "./data"
target_folder <- "./plots"

#Load Vars
x_orig <- readRDS(file.path(folder2use, "NEW_x_orig.rds"))
y <- readRDS(file.path(folder2use, "NEW_y.rds"))

# ============================================================================
# Selection of Basins
# ============================================================================

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge ="on")
red_x_orig <- x_orig[reducer, ]
red_y <-  y[reducer, ]
cols_all <- c(2, 5, 6, 7, 10, 12, 15, 16, 17, 19, 22, 25)
cols_climatic <- c(19, 25)
cols_physiographic <- c(10, 16, 17)
cols_physio_clima <- c(cols_physiographic, cols_climatic)

# creating tuning pars
gamma_with_kmeans <- get_classes_for_gamma(red_y$mean_gamma,
                                           n_centers=3,
                                           print_plot = FALSE)

classified_gamma <-  gamma_with_kmeans[["df"]]
thresholds <- gamma_with_kmeans[["dfThresholds"]]
tune_1 <- thresholds[2, 1] #used in regression models
tune_2 <- thresholds[1, 3] #used in regression models

normalizeMinMax <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

data.training.all <- apply(red_x_orig[,cols_all], 2, normalizeMinMax)
n_clusters_all <- NbClust::NbClust(data=data.training.all, min.nc=2,max.nc=20, method="kmeans")
# majority rule: 3

data.training.p <- apply(red_x_orig[,cols_physiographic], 2, normalizeMinMax)
n_clusters_p <- NbClust::NbClust(data=data.training.p, min.nc=2,max.nc=20, method="kmeans")
# majority rule: 3

data.training.cl <- apply(red_x_orig[,cols_climatic], 2, normalizeMinMax)
n_clusters_cl <- NbClust::NbClust(data=data.training.cl, min.nc=2,max.nc=20, method="kmeans")
# majority rule: 2 (9 votes - 7 for 3)

data.training.p_cl <- apply(red_x_orig[,cols_physio_clima], 2, normalizeMinMax)
n_clusters_p_cl <- NbClust::NbClust(data=data.training.p_cl, min.nc=2,max.nc=20, method="kmeans")
# majority rule: 3

# highly flexible (using 232 clusters) vs. knn
set.seed(123)
ind <- sample(2, nrow(red_x_orig), replace=TRUE, prob=c(0.50, 0.50))

column2use <- cols_all
kmeans <- apply.kmeans(ind,
             x=red_x_orig[,column2use],
             y=red_y[,3],
             normType="MinMax",
             n_centers = ceiling(sum(ind==1)/2))

knn <- apply.knn(ind,
                 x=red_x_orig[,column2use],
                 y=red_y[,3])

kmeans_t <- apply.kmeans(ind,
                       x=red_x_orig[,column2use],
                       y=red_y[,3],
                       normType="MinMax",
                       n_centers = ceiling(sum(ind==1)/2),
                       mod=T, tuningPars=c(tune_1, tune_2))

knn_t <- apply.knn(ind,
                     x=red_x_orig[,column2use],
                     y=red_y[,3],
                     mod=T, tuningPars=c(tune_1, tune_2))

as.data.frame(list("knn"=unlist(knn), "knn_t"= unlist(knn_t),
              "kmean"=unlist(kmeans), "kmeans_t"= unlist(kmeans_t)))
