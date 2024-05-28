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

reducer <- create_subset(MIN_QUALITY, MIN_SIZE)
red_results <- results[reducer,]


#"Gamma histogram plot"
hist_gamma <- ggplot(red_results, aes(x=mean_gamma)) +
  geom_histogram(fill="white", color="black",
                 center=0,
                 breaks=c(0, 1, 2, 3, 4, 5)) +
  labs(x="calibrated Gamma (-)", y = "Count") +
  scale_x_continuous(breaks=c(0.5, 1.5, 2.5, 3.5, 4.5),
                     labels=c("0.1-1",
                              "1.01-2",
                              "2.01-3",
                              "3.01-4",
                              "4.01-5")) +
  theme_classic()


ggsave(file=file.path(TARGETFOLDER, "Figure_1b_histogram_calibrated_gamma.png"),
       plot=hist_gamma,
       width = 10, height = 7, units = "cm",  dpi = 300)
