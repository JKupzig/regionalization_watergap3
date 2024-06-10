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
distance_to_centroid <- readRDS(file.path(ROOT, "NEW_distCentroid.rds"))

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge ="on")
red_x_orig <- x_orig[reducer, ]
red_y <- y[reducer, ]
red_distance_to_centroid <- distance_to_centroid[reducer, reducer]

set.seed(123)
ind_list <- list()
for (i in 1:100){
  ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.50, 0.50))
  ind_list[[i]] <- ind
}
ind <- ind_list[[48]]

template <- NULL

list_to_iterate <- list(
  "CAL"= list(2, "CAL", template),
  "MLRGOOD"=list(2, "MLRGOOD48", template),
  "MLRBAD"=list(2, "MLRBAD48", template),
  "KNNGOOD"=list(2, "KNNGOOD48",template),
  "KNNBAD"=list(2, "KNNBAD48",template),
  "SIGOOD"=list(2, "SIGOOD48",template),
  "SIBAD"=list(2, "SIBAD48",template),
  #"KMEANSGOOD"=list(2, "KMEANSGOOD48",template),
  #"KMEANSBAD"=list(2, "KMEANSBAD48",template),
  "SP"=list(2, "SP48",template),
  "B2B"= list(2, "B2B48", template),
  "DONOR"=list(1, "CAL",template)
)

for (name in names(list_to_iterate)){

  for (cont in c("af","au", "as", "eu", "na", "sa"))
  {

    name_to_read <- list_to_iterate[[name]][[2]]
    ind_to_use <- list_to_iterate[[name]][[1]]
    result_save <- list_to_iterate[[name]][[3]]

    reduced_kge_cont <- read_kge(cont=cont, type=name_to_read,
                                 red_y, red_x_orig, ind, ind_to_use)
    reduced_kge_cont$runtype <- name

    result_save <- rbind(result_save, reduced_kge_cont)
    list_to_iterate[[name]][[3]] <- result_save
  }
}

kge_all <- do.call(rbind, lapply(list_to_iterate, `[[`, 3))

give.n <- function(x){
  return(c(y = 1.0, label = length(x)))
}

kge_all %>%
  merge(., red_y, by.x="station_id", by.y="ID", all.x=T) %>%
  filter(runtype != "DONOR") %>%
  mutate(runtype=factor(runtype, levels=names(list_to_iterate))) %>%
  mutate(gamma_groups = case_when(
  mean_gamma <= .2 ~ ".1 to .2",
  mean_gamma > 0.2 & mean_gamma <= 0.5 ~ ".2 to .5",
  mean_gamma > 0.5 & mean_gamma <= 1 ~ ".5 to 1",
  mean_gamma > 1 & mean_gamma <= 4 ~ "1 to 4",
  mean_gamma > 4.0 & mean_gamma < 5. ~ "4 to 5",
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
  xlab("Gamma") +
  scale_fill_manual(labels=c("CAL (p.-ung.)", "MLR (best)", "MLR (worst)",
                             "knn (best)", "knn (worst)", "SI (best)", "SI (worst)",
                             "SP", "B2B"),
                    values=hcl.colors(length(list_to_iterate)+1, "Spectral")) +
  guides(fill = guide_legend(nrow = 2))

ggsave(file.path("./plots", "Figure_4a.png"),
       width=20, height=16, units="cm", dpi=300)

