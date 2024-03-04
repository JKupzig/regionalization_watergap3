#setwd(r"(C:\Users\jenny\MyProject_sciebo\_Nina\Regionalization\R\data_availability\regionalization_watergap3)")
# set you own working directory (position of run.r) here

# Created plota for Figure 5d

rm(list=ls())

folder2use <- "./data"
TARGETFOLDER <- "./plots"
calibrated_gamma <- raster::brick(file.path(folder2use, "ungauged_region.grd"))
gamma_worldwide <- raster::brick(file.path(folder2use, "gamma_worldwide.grd"))

regionalized_sp <- gamma_worldwide$SP[calibrated_gamma == 0]
regionalized_wg2 <- gamma_worldwide$WG2[calibrated_gamma == 0]
regionalized_new_mlr <- gamma_worldwide$MLR[calibrated_gamma == 0]
calibrated_values <- gamma_worldwide$MLR[calibrated_gamma != 0]


color_1 <- rgb(100, 178, 0, max = 255, alpha = 50, names = "cornflower")
color_2 <- rgb(149, 34, 0, max = 255, alpha = 50, names = "fire")
color_3 <- rgb(237, 34, 255, max = 255, alpha = 50, names = "darkblue")


boxplot_info <- list("WG2"= regionalized_wg2,
                     "NEW"=regionalized_new_mlr,
                     "SP"= regionalized_sp)
calibration <- data.frame(list("name"="Calibrated", 
                               "value"=calibrated_values))

df_boxplot <- data.frame(boxplot_info)
df_boxplot_long <- tidyr::pivot_longer(df_boxplot, cols=c("SP", "WG2", "NEW"))
df_boxplot_long <- rbind(df_boxplot_long, calibration)
df_boxplot_long$name <- factor(df_boxplot_long$name, levels=c("SP", "NEW", "WG2", "Calibrated"))
# sample size
sample_size = df_boxplot_long %>% group_by(name) %>% summarize(num=n())

# Plot
df_boxplot_long %>%
  left_join(sample_size) %>%
  mutate(myaxis = name) %>%
  ggplot( aes(x=myaxis, y=value, fill=name)) +
  geom_violin(width=0.9, size=0.4) +
  geom_boxplot(width=0.5, color="black", alpha=0.2) +
  scale_fill_manual(values = c("SP"="firebrick", 
                               "NEW"="cornflowerblue",
                               "WG2" = "navy", 
                               "Calibrated" = "darkgrey")) +
  theme_bw() +
  theme(legend.position="none") +
  xlab("") +
  ylab("parameter values (-)")

ggsave(file.path(TARGETFOLDER, "Figure_5d_ParameterDistribution.png"), 
         width = 10,
         height = 8,
         units = "cm",
         dpi = 300)
