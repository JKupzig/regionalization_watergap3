# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# Runs split sample test an produces Figure 3 and data for Table 2 a) and b)
# and boxplots for the appendix

rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)

source("./split_sample_test.r")
source("./plotting.r")
source("./helper_functions.r")


# ============================================================================
# Loading Variables (calculated in Prepare_Data at the cluster)
# ============================================================================

MIN_SIZE <- 5000
MIN_QUALITY <- 0.2
SAMPLING_NUMBER <- 10 #here 100 is used in paper

folder2use <- "./data"
target_folder <- "./plots"

#Load Vars
x_orig <- readRDS(file.path(folder2use, "NEW_x_orig.rds"))
y <- readRDS(file.path(folder2use, "NEW_y.rds"))

distance_to_centroid <- readRDS(file.path(folder2use, "NEW_distCentroid.rds"))

# ============================================================================
# Selection of Basins
# ============================================================================

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge ="on")
red_x_orig <- x_orig[reducer, ]
red_y <-  y[reducer, ]

reduces_distance_to_centroid <- distance_to_centroid[reducer, reducer]

#Functions to use gamma
gamma_with_kmeans <- get_classes_for_gamma(red_y$mean_gamma,
                                           n_centers=3,
                                           print_plot = FALSE)

classified_gamma <-  gamma_with_kmeans[["df"]]
thresholds <- gamma_with_kmeans[["dfThresholds"]]
lower <- thresholds[2, 1]
upper <- thresholds[1, 3]


# ============================================================================
# Split Sample Test - have to verify selection!
# ============================================================================

descriptor_sets = list(
  "p+cl"= list(
    columns = c(2, 5, 6, 7, 10, 12, 15, 16, 17, 19, 22, 25),
    result_array = NULL,
    result_df = NULL),
  "subset"= list(
    columns = c(10, 16, 17, 19, 25),
    result_array = NULL,
    result_df = NULL),
  "p"= list(
    columns = c(2, 5, 6, 7, 10, 12, 15, 16, 17),
    result_array = NULL,
    result_df = NULL),
  "cl"= list(
    columns = c(19, 22, 25),
    result_array = NULL,
    result_df = NULL)
  )

# for (subset in names(descriptor_sets)){
#   columns <- descriptor_sets[[subset]][["columns"]]

#   print(sprintf("%s: %s", subset, paste(names(red_x_orig)[columns], collapse=" ")))
#   array_all <- split_sample_test(
#     nrepeats = SAMPLING_NUMBER,
#     column_idx = columns,
#     tuning_pars = c(lower, upper),
#     catchment_distances = reduces_distance_to_centroid,
#     catchment_characteristics = red_x_orig,
#     catchment_gamma = classified_gamma$gamma)

#   type_all <- result_array_to_df(array_all)
#   type_all$info <- subset

#   descriptor_sets[[subset]][["result_array"]] <- array_all
#   descriptor_sets[[subset]][["result_df"]] <- type_all
# }

# array_all <- descriptor_sets[["p+cl"]][["result_array"]]
# array_physio_climatic <- descriptor_sets[["subset"]][["result_array"]]
# array_climatic_all <- descriptor_sets[["cl"]][["result_array"]]
# array_physio_all <- descriptor_sets[["p"]][["result_array"]]
# array_physio_np_cl <- descriptor_sets[["p_no_cl"]][["result_array"]]

# type_all <- descriptor_sets[["p+cl"]][["result_df"]]
# type_physio_climatic <- descriptor_sets[["subset"]][["result_df"]]
# type_climatic_all <- descriptor_sets[["cl"]][["result_df"]]
# type_physio_all <- descriptor_sets[["p"]][["result_df"]]

# # ============================================================================
# # Saving information for appendix
# # ============================================================================
# saveRDS(type_all, "./data/run_results/type_all.rds")
# saveRDS(type_climatic_all, "./data/run_results/type_climatic.rds")
# saveRDS(type_physio_all, "./data/run_results/type_physio.rds")
# saveRDS(type_physio_climatic, "./data/run_results/type_physio_climatic.rds")

# saveRDS(array_all, "./data/run_results/array_all.rds")
# saveRDS(array_climatic_all, "./data/run_results/array_climatic.rds")
# saveRDS(array_physio_all, "./data/run_results/array_physio.rds")
# saveRDS(array_physio_climatic, "./data/run_results/array_physio_climatic.rds")

type_all <- readRDS("./data/run_results/type_all.rds")
type_climatic_all <- readRDS("./data/run_results/type_climatic.rds")
type_physio_all <- readRDS("./data/run_results/type_physio.rds")
type_physio_climatic <- readRDS("./data/run_results/type_physio_climatic.rds")

array_all <- readRDS("./data/run_results/array_all.rds")
array_climatic_all <- readRDS("./data/run_results/array_climatic.rds")
array_physio_all <- readRDS("./data/run_results/array_physio.rds")
array_physio_climatic <- readRDS("./data/run_results/array_physio_climatic.rds")


# ============================================================================
# Analysing Tuning effect!
# ============================================================================

array_delta_all <- array_all
array_delta_physio_climatic <- array_physio_climatic
array_delta_climatic_all <- array_climatic_all
array_delta_physio_all <- array_physio_all

ref <- 2 #2 = limited
minus <- 4 #4 = tuned

array_delta_all[,,2] <- (array_delta_all[,,ref] - array_delta_all[,,minus])
array_delta_physio_climatic[,,2] <- (array_delta_physio_climatic[,,ref] - array_delta_physio_climatic[,,minus])
array_delta_climatic_all[,,2] <- (array_delta_climatic_all[,,ref] - array_delta_climatic_all[,,minus])
array_delta_physio_all[,,2] <- (array_delta_physio_all[,,ref] - array_delta_physio_climatic[,,minus])

delta_all <- result_array_to_evaluate_tuning_df(array_delta_all)
delta_physio_climatic <- result_array_to_evaluate_tuning_df(array_delta_physio_climatic)
delta_physio_all <- result_array_to_evaluate_tuning_df(array_delta_physio_all)
delta_climatic_all <- result_array_to_evaluate_tuning_df(array_delta_climatic_all)


delta_all$descriptor <- "p+cl"
delta_physio_climatic$descriptor <- "subset"
delta_climatic_all$descriptor <- "cl"
delta_physio_all$descriptor <- "p"

delta_tuning <- rbind(delta_all,
                      delta_physio_climatic,
                      delta_climatic_all, delta_physio_all)

delta_tuning <- delta_tuning[delta_tuning$type == "orig.",]

color_ramp_all <- brewer.pal(12, "Paired")
color_ramp <- color_ramp_all[c(1:length(descriptor_sets))]
ggplot(delta_tuning) +
  geom_hline(yintercept = 0, col="darkgrey", linewidth=1, lty=2) +
  geom_boxplot(aes(x=method, y=value, fill=descriptor)) +
  theme_bw() +
  scale_fill_manual(name="Descriptor sets", values=color_ramp) +
  scale_x_discrete(labels=c("kmeans", "knn","MLR", "RF","SI", "SI ensemble", "SP", "B2B")) +
  labs(x = "Method", y="logMAE (standard) - logMAE (tuned)")


ggsave(file = file.path(target_folder, "f03.png"),
       width = 25,
       height = 12,
       units = "cm",
       dpi = 300)


# ============================================================================
# Tables in Chapter 3
# ============================================================================

for (idx in c(1,3)){

  if (idx == 3){
    print("tuned version")
  } else {
    print("original")
  }
  median_all <- apply(array_all[,,idx], 2, median)
  median_subset <- apply(array_physio_climatic[,,idx], 2, median)
  median_clima <- apply(array_climatic_all[,,idx], 2, median)
  median_physio <- apply(array_physio_all[,,idx], 2, median)

  idx_val <- idx+1
  val_median_all <- apply(array_all[,,idx_val], 2, median)
  val_median_subset <- apply(array_physio_climatic[,,idx_val], 2, median)
  val_median_clima <- apply(array_climatic_all[,,idx_val], 2, median)
  val_median_physio <- apply(array_physio_all[,,idx_val], 2, median)


  print("getting diffs form val to cal")
  median_overview <- round(rbind(median_clima, median_physio, median_all, median_subset),3)
  val_median_overview <- round(rbind(val_median_clima,
                                     val_median_physio,
                                     val_median_all,
                                     val_median_subset),3)
  diff <- round(median_overview/val_median_overview*100.,1)
  print(diff)

  print("getting bold numbers (bold != 0)")
  benchmark <- apply(array_all[,,2], 2, median)
  val_mask <- apply(val_median_overview, 2, function(x) {benchmark[1] > x})
  mask_for_bold <- median_overview * val_mask
  print(mask_for_bold)

  print("getting ranks to colorize")
  rank_numbers <- apply(val_median_overview[,-c(1, 8)], 2, rank)
  print(rank_numbers)
  sums <- apply(rank_numbers, 1, sum)
  print("##########################################")
}

# ============================================================================
# Plotting Appendix Figures (Boxplots)
# ============================================================================

MIN <- 0.35
MAX <- 0.7
BREAKS <- seq(MIN, MAX, 0.1)
LEGEND_POSITION <- c(0.2, 0.15)
# regression based approaches MLR, RF

independent_from_descriptors <- c("cal_WG2", "val_WG2",
                                  "cal_WG2_t", "val_WG2_t")
shown_sets_first_plot <- c("cal_MLR", "val_MLR" ,
                           "cal_MLR_t", "val_MLR_t",
                           "cal_RF", "val_RF",
                           "cal_RF_t", "val_RF_t",
                           "info")

physio_all_plot <- type_physio_all[names(type_physio_all) %in% shown_sets_first_plot]
climatic_all_plot<-  type_climatic_all[names(type_climatic_all) %in% shown_sets_first_plot]
physio_climatic_first_plot <- type_physio_climatic[names(type_physio_climatic) %in% c(shown_sets_first_plot)]
all_first_plot <- type_all[names(type_all) %in% c(shown_sets_first_plot, independent_from_descriptors)]


beneficial_information <- create_boxplots(
  data_1 = physio_climatic_first_plot,
  data_2 = all_first_plot,
  data_3 = physio_all_plot,
  data_4 = climatic_all_plot)

beneficial_information <- beneficial_information +
  ylim(0.1, MAX) +
  theme_bw() +
  theme(legend.position = LEGEND_POSITION,
        legend.title = element_blank(),
        legend.background=element_blank())

ggsave(file = file.path(target_folder, "fappD1a.png"),
  beneficial_information,
  width = 30,
  height = 12,
  units = "cm",
  dpi = 300)

################################################################################

independent_from_descriptors <- c("cal_WG2", "val_WG2",
                                  "cal_SP_1", "val_SP_1",
                                  "cal_SP_1_t", "val_SP_1_t")

shown_sets_second_plot <- c("cal_kmeans", "val_kmeans",
                            "cal_kmeans_t", "val_kmeans_t",
                            "cal_knn", "val_knn",
                            "cal_knn_t", "val_knn_t",
                            "info")

physio_second_plot <- type_physio_all[names(type_physio_all) %in% shown_sets_second_plot]
climatic_second_plot <-  type_climatic_all[names(type_climatic_all) %in% shown_sets_second_plot]
physio_climatic_second_plot <- type_physio_climatic[names(type_physio_climatic) %in% c(independent_from_descriptors,
                                                                                       shown_sets_second_plot)]
all_second_plot <- type_all[names(type_all) %in% c(shown_sets_second_plot)]

ml_information <- create_boxplots(
  climatic_second_plot,
  physio_second_plot,
  physio_climatic_second_plot,
  all_second_plot)

ml_information <- ml_information +
  ylim(MIN, MAX) +
  scale_y_continuous(breaks=BREAKS,
                     labels=BREAKS) +
  theme_bw() +
  theme(legend.position= LEGEND_POSITION,
        legend.title = element_blank(),
        legend.background=element_blank())

ggsave(file = file.path(target_folder, "fappD1c.png"),
       ml_information,
       width = 30,
       height = 12,
       units = "cm",
       dpi = 300)

################################################################################
independent_from_descriptors <- c("cal_WG2", "val_WG2")

shown_sets_second_plot <- c("cal_SI_1", "val_SI_1",
                            "cal_SI_1_t", "val_SI_1_t",
                            "cal_SI_10", "val_SI_10",
                            "cal_SI_10_t", "val_SI_10_t",
                            "info")

physio_second_plot <- type_physio_all[names(type_physio_all) %in% shown_sets_second_plot]
climatic_second_plot <-  type_climatic_all[names(type_climatic_all) %in% shown_sets_second_plot]
physio_climatic_second_plot <- type_physio_climatic[names(type_physio_climatic) %in% c(independent_from_descriptors,
                                                                                       shown_sets_second_plot)]
all_second_plot <- type_all[names(type_all) %in% c(shown_sets_second_plot)]

ml_information <- create_boxplots(
  climatic_second_plot,
  physio_second_plot,
  physio_climatic_second_plot,
  all_second_plot)

ml_information <- ml_information +
  ylim(MIN, MAX) +
  scale_y_continuous(breaks=BREAKS,
                     labels=BREAKS) +
  theme_bw() +
  theme(legend.position= LEGEND_POSITION,
        legend.title = element_blank(),
        legend.background=element_blank())

ggsave(file = file.path(target_folder, "fappD1b.png"),
       ml_information,
       width = 30,
       height = 12,
       units = "cm",
       dpi = 300)
