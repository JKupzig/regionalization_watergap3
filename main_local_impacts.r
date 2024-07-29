# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# Created subplots for Figure 6 a-c (used in qgis)

rm(list=ls())

source("plotting.r")
library(dplyr)

TARGET <- "./plots"
ROOT <- "./data/LocalData"
X_LABEL <- expression('Discharge (m'^3*'s'^-1*')')
YEARS <- 1980:2016

# Figure 7: b-d
rio_bravo <- read.table(file.path(ROOT, "rio_bravo_discharge_1980_to_2016.txt"))
plot_shadow(rio_bravo,
            ylabel = X_LABEL,
            xlabel = sprintf("Monthly mean (%d-%d)", min(YEARS), max(YEARS)),
            legend = "topleft",
            name = file.path(TARGET, "f06b.png"))

tamar_river <- read.table(file.path(ROOT, "tamar_river_discharge_1980_to_2016.txt"))
plot_shadow(tamar_river,
            ylabel = X_LABEL,
            xlabel = sprintf("Monthly mean (%d-%d)", min(YEARS), max(YEARS)),
            legend = "topleft",
            name = file.path(TARGET,"f06d.png"))

tiber <- read.table(file.path(ROOT, "tiber_discharge_1980_to_2016.txt"))
plot_shadow(tiber,
            ylabel = X_LABEL,
            xlabel = sprintf("Monthly mean (%d-%d)", min(YEARS), max(YEARS)),
            legend = "topleft",
            name = file.path(TARGET, "f06c.png"))
