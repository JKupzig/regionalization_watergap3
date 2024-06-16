# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# Created subplots for Figure 6 and 7

rm(list=ls())

source("plotting.r")
library(dplyr)

TARGET <- "./plots"
ROOT <- "./data/LocalData"
X_LABEL <- expression('Discharge (m'^3*'s'^-1*')')
YEARS <- 1980:2016

# Figure 6: b, c, d
rio_bravo <- read.table(file.path(ROOT, "rio_bravo_discharge_1980_to_2016.txt"))
plot_shadow(rio_bravo,
            ylabel = X_LABEL,
            xlabel = sprintf("Monthly mean (%d-%d)", min(YEARS), max(YEARS)),
            legend = "topleft",
            name = file.path(TARGET,
                             sprintf("riobravo_discharge_%d_to_%d.png", min(YEARS), max(YEARS))))

tamar_river <- read.table(file.path(ROOT, "tamar_river_discharge_1980_to_2016.txt"))
plot_shadow(tamar_river,
            ylabel = X_LABEL,
            xlabel = sprintf("Monthly mean (%d-%d)", min(YEARS), max(YEARS)),
            legend = "topleft",
            name = file.path(TARGET,
                             sprintf("tamar_discharge_%d_to_%d.png", min(YEARS), max(YEARS))))

tiber <- read.table(file.path(ROOT, "tiber_discharge_1980_to_2016.txt"))
plot_shadow(tiber,
            ylabel = X_LABEL,
            xlabel = sprintf("Monthly mean (%d-%d)", min(YEARS), max(YEARS)),
            legend = "topleft",
            name = file.path(TARGET,
                             sprintf("tiber_discharge_%d_to_%d.png", min(YEARS), max(YEARS))))
