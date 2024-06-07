
rm(list=ls())


library(ggplot2)
library(dplyr)
library(tidyr)

source("./algorithm/apply.Beck2016.R")
source("./algorithm/apply.knn.R")
source("./algorithm/apply.kmeans.R")
source("./algorithm/apply.MultipleLinearRegression.R")
source("./algorithm/apply.spatialProximity.R")
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
  "KMEANSGOOD"=list(2, "KMEANSGOOD48",template),
  "KMEANSBAD"=list(2, "KMEANSBAD48",template),
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



create_delta <- function(list_to_iterate,
                         reference_name,
                         attribute_name){

  new_columns_name <- "delta" #sprintf("delta_%s_%s", attribute_name, reference_name)
  reference <- list_to_iterate[[reference_name]][[3]]
  if (is.null(reference)){
    stop("assigned reference name not in list to examine!")
  }

  names2examine <- names(list_to_iterate)[!names(list_to_iterate) %in% c(reference_name, "DONOR")]
  for (name in names2examine){
    values <- list_to_iterate[[name]][[3]]
    values[[new_columns_name]] <- values[[attribute_name]] - reference[[attribute_name]]
    list_to_iterate[[name]][[3]] <- values
  }
  list_to_iterate[[reference_name]][[3]][[new_columns_name]] <- NA
  list_to_iterate[["DONOR"]][[3]][[new_columns_name]] <- NA

  return(list_to_iterate)
}

create_corr_matrix <- function(list_to_iterate, attribute_name, method="pearson"){

  names2examine <- names(list_to_iterate)[!names(list_to_iterate) %in% c("CAL", "DONOR")]

  corr_matrix <- matrix(NA,
                        ncol=length(names2examine),
                        nrow=length(names2examine))
  row <- 0
  for (ref_name in names2examine){
    reference <- list_to_iterate[[ref_name]][[3]]
    row <- row + 1
    col <- 1
    for (name in names2examine){
      values <- list_to_iterate[[name]][[3]]
      pearson <- cor(values[[attribute_name]],
                     reference[[attribute_name]],
                     method =method)
      corr_matrix[row, col] <- round(pearson, 2)
      col <- col + 1
    }
  }

  row.names(corr_matrix) <- names2examine
  colnames(corr_matrix) <- names2examine
  return(corr_matrix)
}

corr_matrix <- create_corr_matrix(list_to_iterate,"KGE", method="kendall")
library(plot.matrix)
par(mar=c(6.1, 7.5, 4.1, 4.1)) # adapt margins
plot(corr_matrix, col=rev(hcl.colors(10, "Viridis")),
     axis.col=list(side=1, las=2), axis.row = list(side=2, las=1),
     ylab='', xlab='',
     main="", digits=3)
devtools::unload('plot.matrix')
dev.off()

list_to_iterate_with_delta <- create_delta(list_to_iterate, "CAL","KGE")
kge_all <- do.call(rbind, lapply(list_to_iterate_with_delta, `[[`, 3))

upper_threshold <- 0.01
lower_threshold <- -0.01
# overlapping_map <- kge_all %>%
#   filter(., runtype != "CAL") %>%
#   filter(., runtype != "DONOR") %>%
#   filter(., delta >= threshold) %>%
#   count(station_id) %>%
#   as.data.frame()


run_type_names <- sort(names(list_to_iterate)[!names(list_to_iterate) %in% c("DONOR", "CAL")])
matrix_piechart <- matrix(NA, nrow=1, ncol=length(run_type_names))
piechart_data <- kge_all %>%
  filter(., runtype !="CAL") %>%
  filter(., runtype !="DONOR") %>%
  #filter(., station_id %in% ids) %>%
  filter(.,delta >= lower_threshold & delta <= upper_threshold) %>%
  count(station_id) %>%
  count(new_n = n) %>%
  as.data.frame()


pie(piechart_data$n,
    labels = piechart_data$n,
    main="",
    col=hcl.colors(length(run_type_names), "Spectral"))

legend("left",
       cex=0.7,
       ncol=1,
       legend = seq(1, length(run_type_names), 1),
       fill = hcl.colors(length(run_type_names), "Spectral"))


interesting_basins <- kge_all %>%
  filter(., runtype!="CAL") %>%
  filter(., runtype!="DONOR") %>%
  filter(.,delta >= lower_threshold & delta <= upper_threshold) %>%
  count(station_id) %>%
  filter(.,n <= 7) %>%
  as.data.frame()

kge_global <- kge_all
kge_global$cont = "global"

give.n <- function(x){
  return(c(y = 1.05, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

kge_all %>%
  rbind(., kge_global) %>%
  merge(., red_y, by.x="station_id", by.y="ID", all.x=T) %>%
  #filter(station_id %in% interesting_basins$station_id) %>%
  filter(runtype != "CAL") %>%
  mutate(cont = factor(cont, levels=c("global", "au", "as", "af", "eu", "na", "sa"))) %>%
  ggplot(.) +
  geom_boxplot(aes(x=cont, y=mean_gamma/5), outlier.shape = 21) +
  geom_boxplot(aes(x=cont, y=KGE, fill=runtype)) +
  stat_summary(aes(x=cont, y=KGE, group=runtype),
               fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  coord_cartesian(ylim = c(0.0, 1.05)) +
  scale_y_continuous(
    name = "KGE",
    sec.axis = sec_axis(trans=~.*5, name="Calibrated Gamma")
  ) +
  theme_bw() +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_fill_manual(values=hcl.colors(length(run_type_names)+1, "Spectral")) +
  guides(fill = guide_legend(nrow = 2))

kge_all %>%
  merge(., red_y, by.x="station_id", by.y="ID", all.x=T) %>%
  #filter(station_id %in% interesting_basins$station_id) %>%
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
  stat_summary(aes(x=gamma_groups, y=KGE, group=runtype),
               fun.data = give.n, geom = "text",
               position = position_dodge(width = 0.75)) +
  coord_cartesian(ylim = c(0.0, 1.05)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_fill_manual(values=hcl.colors(length(run_type_names)+1, "Spectral")) +
  guides(fill = guide_legend(nrow = 2))

ggsave(file.path("./plots", "review_indifferent_basins_map.png"),
       width=24, height=16, units="cm", dpi=300)
