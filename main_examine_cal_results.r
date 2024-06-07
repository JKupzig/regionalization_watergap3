# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

rm(list = ls())

library(ggplot2)
source("./helper_functions.r")

MIN_QUALITY <- 0.2
MIN_SIZE <- 5000
folder2use <- "./data"
TARGETFOLDER <- "./plots"

quality <- readRDS(file.path(folder2use,"NEW_quality_monthly_bias.rds"))
results <- readRDS(file.path(folder2use, "NEW_y.rds"))

nrow(quality) - sum(create_subset(NULL, MIN_SIZE))
nrow(quality) - sum(create_subset(MIN_SIZE, use_kge = "on"))
nrow(quality) - sum(create_subset(MIN_QUALITY, MIN_SIZE, use_kge = "on"))

sum(create_subset(NULL, MIN_SIZE))
reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge = "on")
reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge = "on")

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge = "on")
red_results <- results[reducer,]

hist(red_results$mean_gamma)

#"Gamma histogram plot"
hist_gamma <- ggplot(red_results, aes(x=mean_gamma)) +
  geom_histogram(fill="white", color="black",
                 center=0,
                 breaks=seq(0,5,0.5)) +
  labs(x="calibrated Gamma (-)", y = "Count") +
  scale_x_continuous(breaks=seq(0,4.5,0.5)+0.25,
                     labels=c(".1 to .5",
                              ".5 to 1",
                              "1 to 1.5",
                              "1.5 to 2",
                              "2 to 2.5",
                              "2.5 to 3",
                              "3 to 3.5",
                              "3.5 to 4",
                              "4 to 4.5",
                              "4.5 to 5")) +
  theme_classic()


ggsave(file=file.path(TARGETFOLDER, "Figure_1b_histogram_calibrated_gamma_kge_considered.png"),
       plot=hist_gamma,
       width = 16, height = 10, units = "cm",  dpi = 300)

#"Gamma histogram plot"
hist_gamma <- ggplot(red_results, aes(x=mean_gamma)) +
  geom_histogram(fill="white", color="black",
                 center=0,
                 breaks=seq(0,5,0.1)) +
  labs(x="calibrated Gamma (-)", y = "Count") +
  theme_bw()
