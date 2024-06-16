# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# evaluates kge from split sample test in the context of low quality basins and creates Figure 4b and 4c

rm(list=ls())

source("./helper_functions.r")
library(ggplot2)
library(dplyr)

kge_all <- read.table("./data/KGE.txt", header=T)
kge_all_summarized <- kge_all
kge_all_summarized$cont = "global"
kge_for_plot <- rbind(kge_all, kge_all_summarized)
kge_for_plot$cont <- factor(kge_for_plot$cont,
                            levels=c("global", "af", "as", "au", "eu", "na", "sa"))

overlapping_map <- kge_all %>%
  filter(.,KGE <= 0.2) %>%
  count(station_id) %>%
  as.data.frame()

hist(overlapping_map$n, xlab="#Methods", ylab="#Basin with KGE \u2264 0.2", main="")
dev.copy(png, file.path("./plots", "Figure_4c.png"),
         width=12, height=12, units="cm", res=300)
dev.off()

# El Patatnito (gamma cal = 1.5)
MINUS_CAL_RESULT <- 2
basins_all_not_estimatable <- which(overlapping_map$n == length(unique(kge_for_plot$runtype))-MINUS_CAL_RESULT)
basin_id <- overlapping_map$station_id[basins_all_not_estimatable]
estimates <- readRDS("./data/estimates.rds")
estimates_difficult_basins <- estimates[estimates$basin_id %in% basin_id,]
results_outlier <- kge_all[kge_all$station_id %in% basin_id, ]
print("outlier for all methods:")
print(results_outlier)



kge_analyse <- kge_all %>%
  group_by(runtype) %>%
  summarize(max=quantile(KGE, probs=1.0),
            min=quantile(KGE, probs=0),
            median=quantile(KGE, probs=0.5),
            mean=mean(KGE)) %>%
  as.data.frame()

outliers <- kge_all %>%
  filter(.,KGE <= 0.2) %>%
  group_by(runtype) %>%
  count(runtype) %>%
  as.data.frame()

kge_analyse <- merge(outliers, kge_analyse, by="runtype")
print("data for Table in Figure 4b")
print(kge_analyse)

# pairwise.wilcox.test
kge_all_cal <- kge_all[!kge_all$runtype %in% c("DONOR", "CAL"),]
kge_all_cal$runtype <- as.character(kge_all_cal$runtype)
kge_all_cal$runtype[kge_all_cal$runtype == "SP"] <- "S_P"
wilcox <- pairwise.wilcox.test(kge_all_cal$KGE,
                     kge_all_cal$runtype, paired=T, exact = FALSE,
                     p.adjust="BH")
result <- wilcox$p.value[which(rownames(wilcox$p.value) == "SIGOOD"),]
also_good <- colnames(wilcox$p.value)[which(result >= 0.05)] #null hypothesis cannot be rejected --> no signficance difference in central tendecy
print("equally well-performing methods:")
print(also_good)
