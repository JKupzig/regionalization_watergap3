# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# evaluates kge from split sample test and creates Figure 4a

rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)

source("./helper_functions.r")

#Load Vars
ROOT <- "./data"
MIN_SIZE <- 5000
MIN_QUALITY <- 0.2

x_orig <- readRDS(file.path(ROOT, "NEW_x_orig.rds"))
y <- readRDS(file.path(ROOT, "NEW_y.rds"))
kge_all <- read.table(file.path(ROOT, "KGE.txt"), header=T)

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge ="on")
red_y <- y[reducer, ]


give.n <- function(x){
  return(c(y = 1.0, label = length(x)))
}

gamma_expression <- expression(paste("Calibrated ", gamma, " (-)"))
list_to_iterate <- unique(kge_all$runtype)

kge_all %>%
  merge(., red_y, by.x="station_id", by.y="ID", all.x=T) %>%
  filter(runtype != "DONOR") %>%
  mutate(runtype=factor(runtype, levels=list_to_iterate)) %>%
  mutate(gamma_groups = case_when(
  mean_gamma <= .2 ~ "0.1-0.2",
  mean_gamma > 0.2 & mean_gamma <= 0.5 ~ "0.2-.5",
  mean_gamma > 0.5 & mean_gamma <= 1 ~ "0.5-1",
  mean_gamma > 1 & mean_gamma <= 4 ~ "1-4",
  mean_gamma > 4.0 & mean_gamma < 5. ~ "4-5",
  mean_gamma == 5.0 ~ "5"
  )) %>%
  ggplot(.) +
  geom_boxplot(aes(x=gamma_groups, y=KGE, fill=runtype)) +
  # stat_summary(aes(x=gamma_groups, y=KGE, group=runtype),
  #              fun.data = give.n, geom = "text",
  #              position = position_dodge(width = 0.75)) +
  coord_cartesian(ylim = c(0.2, 1.0)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  xlab(gamma_expression) +
  ylab("KGE (-)") +
  scale_fill_manual(labels=c("CAL (p.-ung.)", "MLR (best)", "MLR (worst)",
                             "knn (best)", "knn (worst)", "SI (best)", "SI (worst)",
                             "SP", "B2B"),
                    values=hcl.colors(length(list_to_iterate)+1, "Spectral")) +
  guides(fill = guide_legend(nrow = 2))

ggsave(file.path("./plots", "Figure_4a.png"),
       width=20, height=16, units="cm", dpi=300)

