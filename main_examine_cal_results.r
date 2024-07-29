# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# creates Figure 1b

rm(list = ls())

library(ggplot2)
source("./helper_functions.r")

MIN_QUALITY <- 0.2
MIN_SIZE <- 5000
folder2use <- "./data"
TARGETFOLDER <- "./plots"

results <- readRDS(file.path(folder2use, "NEW_y.rds"))
reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge = "on")
red_results <- results[reducer,]

#"Gamma histogram plot"
gamma_expression <- expression(paste("Calibrated ", gamma, " (-)"))

hist_gamma <- ggplot(red_results, aes(x=mean_gamma)) +
  geom_histogram(fill="white", color="black",
                 center=0,
                 breaks=seq(0,5,0.5)) +
  labs(x=gamma_expression, y = "Count (-)") +
  scale_x_continuous(breaks=seq(0,4.5,0.5)+0.25,
                     labels=c("0.1-0.5",
                              "0.5-1",
                              "1-1.5",
                              "1.5-2",
                              "2-2.5",
                              "2.5-3",
                              "3-3.5",
                              "3.5-4",
                              "4-4.5",
                              "4.5-5")) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))


ggsave(file=file.path(TARGETFOLDER, "f01b.png"),
       plot=hist_gamma,
       width = 16, height = 10, units = "cm",  dpi = 300)
