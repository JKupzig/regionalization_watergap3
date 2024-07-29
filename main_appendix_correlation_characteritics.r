#setwd(r"(C:\Users\jenny\...")
# set you own working directory here

# Description:
# Script creates append C1 and C2 (Correlation & Distribution plots of
# Catchment Descriptors).

rm(list = ls())

library(ggplot2)
source("./helper_functions.r")

MIN_SIZE <- 5000
MIN_QUALITY <- 0.2
SAMPLING_NUMBER <- 100

folder2use <- "./data"
TARGETFOLDER <- "./plots"

#Load Vars
x_orig <- readRDS(file.path(folder2use, "NEW_x_orig.rds"))
y <- readRDS(file.path(folder2use, "NEW_y.rds"))

# ============================================================================
# Selection of Basins
# ============================================================================

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge = "on")
red_x_orig <- x_orig[reducer, ]
red_y <-  y[reducer, ]


columns2use <- c("mean_smax", "mean_op_water", "mean_wetland", "areaBasin",
  "mean_slope", "mean_altitude", "mean_sealedArea", "mean_Forest",
  "mean_permaglac", "sum_sw", "sum_prec", "mean_temp")

labels2use <- c("mean_smax" = "Soil Storage",
               "mean_op_water" = "Open Water Bodies",
               "mean_wetland" = "Wetlands",
               "areaBasin" = "Size",
               "mean_slope" = "Slope",
               "mean_altitude" = "Altitude",
               "mean_sealedArea" = "Sealed Area",
               "mean_Forest" = "Forest",
               "mean_permaglac" = "Permafrost & Glacier",
               "mean_temp" = "Mean Temperature",
               "sum_prec" = "Yearly Precipitation",
               "sum_sw" = "Yearly Shortwave Downward Radiation",
               "gamma" = "Calibration Parameter")

labels2use_wunits <- c("Soil~Storage~(mm)",
                "Open~Water~Bodies~('%')",
                "Wetlands~('%')",
                "Size~(km^{2})",
                "Slope~(cf.~Döll~'&'~Fiedler~2008)",
                "Altitude~(m.a.s.l)",
                "Sealed~Area~('%/100')",
                "Forest~('%')",
                "Permafrost~'&'~Glacier~('%')",
                "Yearly~Shortwave~Downward~Radiation~(Wm^{-2})",
                "Yearly~Precipitation~(mm)",
                "Mean~Temperature~('°C')")

df2examine <- red_x_orig[, names(red_x_orig) %in% columns2use]
df2examine$gamma <- red_y$mean_gamma #add gamma


df2plot <- df2examine
count <- 1
for (name in colnames(df2plot)){
  colnames(df2plot)[count] <- labels2use[[name]]
  count <- count + 1
}

png(file = file.path(TARGETFOLDER, "fappC1.png"),
    res = 300, units = "cm", height=32, width = 25)
correlation_matirx <- cor(df2plot)
corrplot::corrplot(correlation_matirx, order = 'AOE', type = 'lower', diag = FALSE,
                   addCoef.col = 'black', number.cex = 0.8,
                   tl.col = 'black', tl.srt = 65, tl.cex = 0.95)

dev.off()


################

basin_info_long <- tidyr::pivot_longer(df2examine, cols = all_of(columns2use))
basin_info_long$name <-  factor(basin_info_long$name,
                                levels = columns2use,
                                labels = labels2use_wunits)

ggplot() +
  geom_histogram(basin_info_long, mapping = aes(value),
                 alpha = 1, fill = "cornflowerblue") +
  facet_wrap(~name, scales = "free_x",
             labeller = label_parsed) +
  theme_bw() +
  ylab("Count (-)") +
  xlab("Descriptor value")

ggsave(file.path(TARGETFOLDER, "fappC2.png"),
       device = "png",
       width = 30, height = 25,
       units = "cm",
       dpi = 300)
