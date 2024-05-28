
source("./helper_functions.r")
library(ggplot2)

#Load Vars
ROOT <- "./data"
MIN_SIZE <- 5000
MIN_QUALITY <- 0.2

x_orig <- readRDS(file.path(ROOT, "NEW_x_orig.rds"))
y <- readRDS(file.path(ROOT, "NEW_y.rds"))

reducer <- create_subset(MIN_QUALITY, MIN_SIZE, use_kge ="off")
red_x_orig <- x_orig[reducer, ]
red_y <- y[reducer, ]

set.seed(123)
ind <- sample(2, nrow(red_x_orig), replace = TRUE, prob = c(0.50, 0.50))

cont = "au"

kge_sp <- read.table(sprintf("./data/KGE_SP_%s.txt", cont), sep="\t", header=T)
kge_mlr <- read.table(sprintf("./data/KGE_MLR_%s.txt", cont), sep="\t", header=T)
kge_b2b <- read.table(sprintf("./data/KGE_B2B_%s.txt", cont), sep="\t", header=T)
kge_cal <- read.table(sprintf("./data/KGE_CAL_%s.txt", cont), sep="\t", header=T)

read_kge <- function(cont, type, red_y, red_x_orig){
  kge_cal_cont <- read.table(sprintf("./data/KGE_%s_%s.txt", type, cont), sep="\t", header=T)
  kge_cal_cont$cont <- cont
  kge_cal_cont$runtype <- type

  missing_basins <- red_y[!(red_y$ID %in% kge_cal_cont$station_id) & red_x_orig$continent == cont,1]
  if (length(missing_basins) > 0){
    stop(sprintf("something wrong with basins in %s!", cont))
  }

  reduced_kge_cont <- kge_cal_cont[kge_cal_cont$station_id %in% red_y$ID[ind==2],]
  return(reduced_kge_cont)
}

kge_cal <- NULL
kge_mlr <- NULL
kge_b2b <- NULL
kge_sp <- NULL
for (cont in c("au", "af", "as", "eu", "na", "sa"))
{
  reduced_kge_cont <- read_kge(cont=cont, type="CAL", red_y, red_x_orig)
  kge_cal <- rbind(kge_cal, reduced_kge_cont)
}

for (cont in c("au")){
  reduced_kge_cont <- read_kge(cont=cont, type="MLR", red_y, red_x_orig)
  kge_mlr <- rbind(kge_mlr, reduced_kge_cont)

  reduced_kge_cont <- read_kge(cont=cont, type="B2B", red_y, red_x_orig)
  kge_b2b <- rbind(kge_b2b, reduced_kge_cont)

  reduced_kge_cont <- read_kge(cont=cont, type="SP", red_y, red_x_orig)
  kge_sp <- rbind(kge_sp, reduced_kge_cont)
}


kge_all <- rbind(kge_cal, kge_sp, kge_b2b, kge_mlr)
kge_all$runtype <- factor(kge_all$runtype, levels=c("CAL", "MLR", "SP", "B2B"))
ggplot(kge_all,
       aes(x=cont,
           y=KGE,
           fill=runtype)) +
  scale_fill_manual(
    labels = c("Calibrated","tuned MLR", "SP", "WG2"),
    values = list(
      "B2B"="navy",
      "CAL"="darkgrey",
      "MLR"="cornflowerblue",
      "SP"="firebrick"
    )) +
  geom_boxplot() +
  theme_bw()
