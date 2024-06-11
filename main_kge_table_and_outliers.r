# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# evaluates kge from split sample test in the context of low quality basins and creates Figure 4b and 4c

rm(list=ls())

source("./helper_functions.r")
library(ggplot2)
library(dplyr)

#Load Vars
ROOT <- "./data"
MIN_SIZE <- 5000
MIN_QUALITY <- 0.2

x_orig <- readRDS(file.path(ROOT, "NEW_x_orig.rds"))
y <- readRDS(file.path(ROOT, "NEW_y.rds"))

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge ="on")
red_x_orig <- x_orig[reducer, ]
red_y <- y[reducer, ]

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
  "SP"=list(2, "SP48",template),
  "B2B"= list(2, "B2B48", template),
  #"KMEANSGOOD"=list(2, "KMEANSGOOD48",template),
  #"KMEANSBAD"=list(2, "KMEANSBAD48",template),
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

remove_null_from_list <- function(list_to_analyse){
  count <- 1
  list_without_null <- list()
  for (i in 1:length(list_to_analyse)){
    if (!is.null(list_to_analyse[[i]])){
      list_without_null[[count]] <- list_to_analyse[[i]]
      count <- count + 1
    }
  }
  return(list_without_null)
}

kge_all <- do.call(rbind, lapply(list_to_iterate, `[[`, 3))
kge_all_summarized <- kge_all
kge_all_summarized$cont = "global"
kge_for_plot <- rbind(kge_all, kge_all_summarized)
kge_for_plot$cont <- factor(kge_for_plot$cont,
                            levels=c("global", "af", "as", "au", "eu", "na", "sa"))

overlapping_map <- kge_all %>%
  filter(.,KGE <= 0.2) %>%
  count(station_id) %>%
  as.data.frame()

# El Patatnito (gamma cal = 1.5)
basins_all_not_estimatable <- which(overlapping_map$n == length(names(list_to_iterate))-2)
basin_id <- overlapping_map$station_id[basins_all_not_estimatable]
estimates <- readRDS("./data/estimates.rds")
estimates_difficult_basins <- estimates[estimates$basin_id %in% basin_id,]
kge_all[kge_all$station_id %in% basin_id, ]


outliers <- kge_all %>%
  filter(.,KGE <= 0.2) %>%
  group_by(runtype) %>%
  count(runtype) %>%
  as.data.frame()

hist(overlapping_map$n, xlab="#methods", ylab="#basin with KGE \u2264 0.2", main="")
dev.copy(png, file.path("./plots", "Figure_4c.png"),
         width=12, height=12, units="cm", res=300)
dev.off()


# data for Table in Figure 4b
kge_analyse <- kge_all %>%
  filter(., !runtype %in% c("KMEANSGOOD", "KMEANSBAD")) %>%
  group_by(runtype) %>%
  summarize(max=quantile(KGE, probs=1.0),
            min=quantile(KGE, probs=0),
            median=quantile(KGE, probs=0.5),
            mean=mean(KGE)) %>%
  as.data.frame()
kge_analyse[,c(2,4,3, 5)] <- round(kge_analyse[,c(2,4,3,5)], 3)

merge(outliers, kge_analyse, by="runtype")

# pairwise.wilcox.test --> argument to use allregionalization methods!
kge_all_cal <- kge_all[!kge_all$runtype %in% c("DONOR", "CAL"),]
kge_all_cal$runtype <- as.character(kge_all_cal$runtype)
pairwise.wilcox.test(kge_all_cal$KGE,
                     kge_all_cal$runtype, paired=T, exact = FALSE,
                     p.adjust="bonferroni")

pairwise.wilcox.test(kge_all_cal$KGE,
                     kge_all_cal$runtype, paired=T, exact = FALSE,
                     p.adjust="BH")

pairwise.wilcox.test(kge_all_cal$KGE,
                     kge_all_cal$runtype, paired=T, exact = FALSE,
                     p.adjust="BY")
