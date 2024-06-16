#setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# Description:
# Script runs correlation and entropy analysis and returns Table 1 as two different plots:
# 1a: Informiton about the distribution of catchment descriptors
# 1b: information about correlation and entropy analysis

rm(list = ls())

library(tidyr)
library(dplyr)
library(grid)
source("./helper_functions.r")


# ============================================================================
# Preparing data to analyse
# ============================================================================
MIN_QUALITY <- 0.2
MIN_SIZE <- 5000

folder2use <- "./data"
target_folder <- "./plots"

x_orig <- readRDS(file.path(folder2use, "NEW_x_orig.rds"))
y <-  readRDS(file.path(folder2use, "NEW_y.rds"))

#define table
tt2 <- gridExtra::ttheme_default(
  core = list(fg_params = list(hjust = 1, x = 0.9)),
  rowhead = list(fg_params = list(hjust = 1, x = 0.95)))

# ============================================================================
# Table 1: Catchment Descriptor overview
# ============================================================================

functions <- list(
  min = ~min(.x, na.rm = TRUE),
  max = ~max(.x, na.rm = TRUE),
  mean = ~mean(.x, na.rm = TRUE),
  median = ~median(.x, na.rm = TRUE)
)

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge = "on")
red_x_orig <- x_orig[reducer, ]
red_y <- y[reducer, ]
df4table <- red_x_orig[, names(red_x_orig) %in% columns2use]

funcs <- "mean"
dfs <- df4table %>%
  summarise(across(everything(), functions)) %>%
  pivot_longer(cols = ends_with(funcs), names_to = "attribute",
               names_pattern = sprintf("(.*)_%s", funcs),
               values_to = "funcs") %>%
  select(attribute)

for (funcs in names(functions)) {
  df_reshaped_funcs <- df4table %>%
    summarise(across(everything(), functions)) %>%
    pivot_longer(cols = ends_with(funcs), names_to = "attribute",
                 names_pattern = sprintf("(.*)_%s", funcs),
                 values_to = "funcs") %>%
    select(funcs) %>%
    mutate_if(is.numeric, round, 3)

  dfs <- cbind(dfs, df_reshaped_funcs)
}

names(dfs) <- c("attribute", names(functions))


my_table <- gridExtra::tableGrob(dfs, theme=tt2, rows = NULL)
grid::grid.newpage()
grid::grid.draw(my_table)
ggplot2::ggsave(
  file = file.path(target_folder, "Table_1a_attribute_info.png"),
  my_table, width = 14, height = 10, units = "cm")


# ============================================================================
# compute Shannon entropy
# ============================================================================

#compute Shannon entropy
entropy <- function(target) {
  freq <- table(target) / length(target)
  vec <- as.data.frame(freq)[, 2]  # vectorize
  vec <- vec[vec > 0] #drop 0 to avoid NaN resulting from log2
  return(-sum(vec * log2(vec))) #compute entropy
}

#entropy = 0, not much information / similar infomration
# the greater e the more information it contains
entropy_gamma <- entropy(y$mean_gamma)

# ============================================================================
# compute information gain using Shannon entropy
# ============================================================================


#Information gain
#returns IG for numerical variables.
information_gain <- function(data, feature, target, bins = 4) {

  data <- data[!is.na(data[, feature]), ] #Strip out rows where feature is NA
  e0 <- entropy(data[, target]) #compute entropy for the parent
  data$cat <- cut(data[, feature], breaks = bins, labels = c(1:bins))
  dd_data <- data %>% group_by(cat) %>%
    summarise(e = entropy(get(target)),
      n = length(get(target)),
      min = min(get(feature)),
      max = max(get(feature))
    )

  dd_data$p <- dd_data$n / nrow(data) #calculate p for each value of feature
  information_gain <- e0 - sum(dd_data$p * dd_data$e) #compute IG
  return(information_gain)
}

data <- cbind(red_y, red_x_orig)
information_gain_vector <- c()
for (name in columns2use){
  information_gain_value <- information_gain(data, name, "mean_gamma", bins=10)
  information_gain_vector <- c(information_gain_vector, information_gain_value)
}

information_gain_df <- data.frame(name=columns2use, IG=information_gain_vector)
information_gain_df <- information_gain_df[order(information_gain_df$IG, decreasing = TRUE), ]
information_gain_df$perc <- information_gain_df$IG / rep(entropy_gamma, nrow(information_gain_df)) * 100


# ============================================================================
# correlation
# ============================================================================

df2examine <- red_x_orig[, names(red_x_orig) %in% columns2use]
df2examine$gamma <- red_y$mean_gamma #add gamma

pearson <- cor(df2examine, method = "pearson")
correlation <- pearson[which(colnames(pearson) == "gamma"), ]
correlation_df <- get_data_frame(correlation, columns2use, "pearson")

pearson <- cor(df2examine, method = "spearman")
correlation <- pearson[which(colnames(pearson) == "gamma"), ]
correlation_df2 <- get_data_frame(correlation, columns2use, "spearman")

pearson <- cor(df2examine, method = "kendall")
correlation <- pearson[which(colnames(pearson) == "gamma"), ]
correlation_df3 <- get_data_frame(correlation, columns2use, "kendall")

new_result <- merge(information_gain_df, correlation_df, by.x = "name", by.y = "name")
new_result <- merge(new_result, correlation_df2, by.x = "name", by.y = "name")
new_result <- merge(new_result, correlation_df3, by.x = "name", by.y = "name")
new_result <- new_result[order(new_result$IG, decreasing = TRUE), ]
new_result[, c(2, 3, 4, 5, 6)] <- round(new_result[, c(2, 3, 4, 5, 6)], 2)

plot(abs(new_result$pearson), new_result$perc)

my_table <- gridExtra::tableGrob(new_result[, c(1,3,4,5,6)], theme = tt2, rows = NULL)
grid.newpage()
grid.draw(my_table)
ggsave(file = file.path(target_folder, "Table_1b_entropy_correlation.png"),
  my_table, width = 14, height = 10, units = "cm")

