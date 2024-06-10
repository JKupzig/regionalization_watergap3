# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# evaluates kge from split sample test and creates Figure 4a

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
ind <- ind_list[[48]] #representative split sample test

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

create_improvement <- function(list_to_iterate,
                               reference_name,
                               attribute_name,
                               to_add = 0)
{
  reference <- list_to_iterate[[reference_name]][[3]]
  if (is.null(reference)){
    stop("assigned reference name not in list to examine!")
  }

  percent_basins_better_or_equal <- c()
  names2examine <- names(list_to_iterate)[!names(list_to_iterate) %in% c(reference_name, "DONOR")]
  for (name in names2examine){
    values <- list_to_iterate[[name]][[3]]
    better <- sum(abs(1 - values[[attribute_name]]) <= (abs(1 - reference[[attribute_name]])+to_add))
    percent_better <- better/length(values[[attribute_name]])
    percent_basins_better_or_equal <- c(percent_basins_better_or_equal, percent_better)
  }

  improvements <- data.frame(runtypes=names2examine,
             percent_better = percent_basins_better_or_equal)

  return(improvements[order(improvements$percent_better,
                            decreasing=T),])
}

create_delta <- function(list_to_iterate,
                         reference_name,
                         attribute_name){

  new_columns_name <- sprintf("delta_%s_%s", attribute_name, reference_name)
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


create_improvement(list_to_iterate, "CAL",  "a",to_add=0.05)
list_to_iterate_with_delta <- create_delta(list_to_iterate, "B2B","KGE")

kge_all <- do.call(rbind, lapply(list_to_iterate_with_delta, `[[`, 3))
kge_all_summarized <- kge_all
kge_all_summarized$cont = "global"
kge_for_plot <- rbind(kge_all, kge_all_summarized)

kge_for_plot$cont <- factor(kge_for_plot$cont,
                            levels=c("global", "af", "as", "au", "eu", "na", "sa"))
kge_for_plot$runtype <- factor(kge_for_plot$runtype,
                          levels=c("DONOR", "CAL",
                                   "KNNGOOD", "KNNBAD",
                                   "KMEANSGOOD", "KMEANSBAD",
                                   "SIGOOD", "SIBAD",
                                   "MLRGOOD", "MLRBAD",
                                   "SP",
                                   "B2B"))

build_colorramp <- c("lightgrey", "darkgrey", hcl.colors(10, "Spectral"))

kge_for_plot <- kge_for_plot[kge_for_plot$cont == "global",]
ggplot(kge_for_plot,
       aes(x=cont,
           y=KGE,
           fill=runtype)) +
  scale_fill_manual(
    values = build_colorramp) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0.0, 1.0)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 2))


ggsave(file.path("./plots", "review_kge_boxplots.png"),
       width=16, height=16, units="cm", dpi=300)


# create global plot and analyse outliers!
kge_all %>%
  group_by(runtype) %>%
  summarize(outlier = sum(KGE <= 0.2))

overlapping_map <- kge_all %>%
  filter(.,KGE <= 0.2) %>%
  count(station_id) %>%
  as.data.frame()

overlapping_map_better <- kge_all %>%
  filter(., runtype != "CAL") %>%
  filter(.,delta_KGE_B2B >= 0.1) %>%
  count(station_id) %>%
  as.data.frame()

overlapping_map_worse <- kge_all %>%
  filter(., runtype != "CAL") %>%
  filter(.,delta_KGE_B2B <= -0.1) %>%
  count(station_id) %>%
  as.data.frame()

gauged_basins <- unique(kge_all$station_id[kge_all$runtype=="DONOR"])
psuedogauged_basins <- unique(kge_all$station_id[kge_all$runtype=="CAL"])

#reading base map
CALIB_FOLDER <- r"(C:/Users/jenny/MyProject_sciebo/_Nina/Regionalization/_Data/2nd_Calibration)"
BASEMAP <- "C:/Users/jenny/MyProject_sciebo/SensitivityAnalysis/ne_110m_land"
wmap <- rgdal::readOGR(dsn=BASEMAP, layer=basename(BASEMAP))
wmap_robin <- sp::spTransform(wmap, sp::CRS("+proj=robin"))


j <- 1
better_basins_list <- list()
worse_basins_list <- list()
bad_basins_list <- list()

gauged_basins_list <- list()
ungauged_basins_list <- list()
cont_id <- 1
for (cont in c("sa", "af", "as", "au", "eu", "na")) {

  rlog::log_info(sprintf("starting continent %s", cont))

  RasterTempl <- watergap3data::GCRC_list[[cont]]
  crs2use <- raster::crs(RasterTempl)

  name_basins <- sprintf("G_BASCALIB_GRDC_%s.UNF0", cont)

  #getting bad catchments
  basins <- as.vector(watergap3data::unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
  basins[(!basins %in% overlapping_map$station_id) & (!is.na(basins))] <- NA
  for (bad_basin in overlapping_map$station_id){
    value2set <- overlapping_map$n[overlapping_map$station_id == bad_basin]
    basins[(basins == bad_basin) & (!is.na(basins))] <- value2set
  }

  if (!sum(is.na(basins)) == length(basins)){
    basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
    basins_polygons <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)
    bad_basins_list[[cont_id]] <- basins_polygons
  }

  #getting better catchments
  basins <- as.vector(watergap3data::unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
  basins[(!basins %in% overlapping_map_better$station_id) & (!is.na(basins))] <- NA
  for (bad_basin in overlapping_map_better$station_id){
    value2set <- overlapping_map_better$n[overlapping_map_better$station_id == bad_basin]
    basins[(basins == bad_basin) & (!is.na(basins))] <- value2set
  }

  if (!sum(is.na(basins)) == length(basins)){
    basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
    basins_polygons <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)
    better_basins_list[[cont_id]] <- basins_polygons
  }

  #getting worse catchments
  basins <- as.vector(watergap3data::unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
  basins[(!basins %in% overlapping_map_worse$station_id) & (!is.na(basins))] <- NA
  for (bad_basin in overlapping_map_worse$station_id){
    value2set <- overlapping_map_worse$n[overlapping_map_worse$station_id == bad_basin]
    basins[(basins == bad_basin) & (!is.na(basins))] <- value2set
  }

  if (!sum(is.na(basins)) == length(basins)){
    basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
    basins_polygons <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)
    worse_basins_list[[cont_id]] <- basins_polygons
  }


  if (!file.exists("./data./gauged_basins_world.rds")){
    #getting gauged catchments
    basins <- as.vector(watergap3data::unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
    basins[!(basins %in% gauged_basins) & (!is.na(basins))] <- NA
    basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
    basins_polygons <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)
    gauged_basins_list[[cont_id]] <- basins_polygon
  }

  if (!file.exists("./data./ungauged_basins_world.rds")){
    #getting ungauged catchments
    basins <- as.vector(watergap3data::unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
    basins[(!basins %in% psuedogauged_basins) & (!is.na(basins))] <- NA
    basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
    basins_polygons <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)
    ungauged_basins_list[[cont_id]] <- basins_polygons
  }
  cont_id <- cont_id + 1
}

if (!file.exists("./data./gauged_basins_world.rds")){
  gauged_basins_world <- do.call("rbind", gauged_basins_list)
  saveRDS(gauged_basins_world, "./data./gauged_basins_world.rds")
} else {
  gauged_basins_world <- readRDS("./data./gauged_basins_world.rds")
}

if (!file.exists("./data./ungauged_basins_world.rds")){
  ungauged_basins_world <- do.call("rbind", ungauged_basins_list)
  saveRDS(ungauged_basins_world, "./data./ungauged_basins_world.rds")
} else {
  ungauged_basins_world <- readRDS("./data./ungauged_basins_world.rds")
}


bad_basins_without_null <- remove_null_from_list(bad_basins_list)
bad_basins_world <- do.call("rbind", bad_basins_without_null)
bad_basins_world$layer <- factor(bad_basins_world$layer,
                                 levels=c(1,2,3,4,5,6,7,8,9,10))
ggplot() +
  ggspatial::geom_sf() +
  ggspatial::geom_sf(data=sf::st_as_sf(wmap_robin), fill=NA, col="black", size=0.25) +
  ggspatial::geom_sf(data=sf::st_as_sf(ungauged_basins_world), fill=rev(hcl.colors(12, "Viridis"))[1]) +
  ggspatial::geom_sf(data=sf::st_as_sf(gauged_basins_world), fill="lightgrey") +
  ggspatial::geom_sf(data=sf::st_as_sf(bad_basins_world),
                                       aes(fill=layer)) +
  scale_fill_manual(values=rev(hcl.colors(12, "Viridis"))[3:12]) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

ggsave(file.path("./plots", "review_kge_map.png"),
       width=24, height=16, units="cm", dpi=300)

par(mfrow=c(1,1))
par(bg=NA)
hist(overlapping_map$n, xlab="number of basins", ylab="KGE \u2264 0.2", main="")
dev.copy(png, file.path("./plots", "review_kge_map_hist.png"),
         width=12, height=12, units="cm", res=300)
dev.off()



good_basins_without_null <- remove_null_from_list(better_basins_list)
good_basins_world <- do.call("rbind", good_basins_without_null)
good_basins_world$layer <- factor(good_basins_world$layer,
                                 levels=c(1,2,3,4,5,6,7,8,9,10))
ggplot() +
  ggspatial::geom_sf() +
  ggspatial::geom_sf(data=sf::st_as_sf(wmap_robin), fill=NA, col="black", size=0.25) +
  ggspatial::geom_sf(data=sf::st_as_sf(ungauged_basins_world), fill=rev(hcl.colors(12, "Viridis"))[1]) +
  ggspatial::geom_sf(data=sf::st_as_sf(gauged_basins_world), fill="lightgrey") +
  ggspatial::geom_sf(data=sf::st_as_sf(good_basins_world),
                     aes(fill=layer)) +
  scale_fill_manual(values=rev(hcl.colors(12, "Viridis"))[3:12]) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

ggsave(file.path("./plots", "review_kge_map_better.png"),
       width=24, height=16, units="cm", dpi=300)



overlapping_map_better$calibrated_gamma <- red_y$mean_gamma[red_y$ID %in% overlapping_map_better$station_id]
boxplot(overlapping_map_better$calibrated_gamma ~ overlapping_map_better$n)
table(overlapping_map_better$n)

overlapping_map_worse$calibrated_gamma <- red_y$mean_gamma[red_y$ID %in% overlapping_map_worse$station_id]
boxplot(overlapping_map_worse$calibrated_gamma ~ overlapping_map_worse$n)
table(overlapping_map_worse$n)

ranks <- kge_all %>%
  filter(., !runtype %in% c("CAL", "DONOR")) %>%
  group_by(station_id) %>%
  mutate(rank = dense_rank(desc(KGE))) %>%
  group_by(runtype) %>%
  summarize(sum(rank)) %>%
  as.data.frame()
par(las=2)
par(mar=c(8,8,1,1)) # adjust as needed
plot(ranks$`sum(rank)`, xaxt = 'n')
axis(side=1,at=seq(1,10),labels=ranks$runtype)
dev.off()



