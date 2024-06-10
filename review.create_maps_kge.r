
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

list_to_iterate_with_delta <- create_delta(list_to_iterate, "CAL", "KGE")
kge_all <- do.call(rbind, lapply(list_to_iterate_with_delta, `[[`, 3))
kge_all_summarized <- kge_all
kge_all_summarized$cont = "global"
kge_for_plot <- rbind(kge_all, kge_all_summarized)
kge_for_plot$cont <- factor(kge_for_plot$cont,
                            levels=c("global", "af", "as", "au", "eu", "na", "sa"))

# create global plot and analyse outliers!
kge_all %>%
  group_by(runtype) %>%
  summarize(outlier = sum(KGE <= 0.2))

overlapping_map <- kge_all %>%
  filter(.,KGE <= 0.2) %>%
  count(station_id) %>%
  as.data.frame()

lower_threshold <- -0.02
upper_threshold <- 0.02
indifferent_basins <- kge_all %>%
  filter(., runtype !="CAL") %>%
  filter(., runtype !="DONOR") %>%
  filter(., delta >= lower_threshold & delta <= upper_threshold) %>%
  count(station_id) %>%
  filter(.,n == 10) %>%
  as.data.frame()


estimates <- readRDS("./data/estimates.rds")
estimates_indifferent <- estimates[estimates$basin_id %in% indifferent_basins$station_id,]
estimates_indifferent$CV <- apply(estimates_indifferent[-c(12)], 1, function(x) { sd(x)/mean(x)})


gauged_basins <- unique(kge_all$station_id[kge_all$runtype=="DONOR"])
psuedogauged_basins <- unique(kge_all$station_id[kge_all$runtype=="CAL"])

#reading base map
CALIB_FOLDER <- r"(C:/Users/jenny/MyProject_sciebo/_Nina/Regionalization/_Data/2nd_Calibration)"
BASEMAP <- "C:/Users/jenny/MyProject_sciebo/SensitivityAnalysis/ne_110m_land"
wmap <- rgdal::readOGR(dsn=BASEMAP, layer=basename(BASEMAP))
wmap_robin <- sp::spTransform(wmap, sp::CRS("+proj=robin"))


j <- 1
indifferent_basins_list <- list()
bad_basins_list <- list()
gamma_basins_list <- list()
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

  #getting indifferent catchments
  basins <- as.vector(watergap3data::unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
  basins[(!basins %in% indifferent_basins$station_id) & (!is.na(basins))] <- NA
  for (bad_basin in indifferent_basins$station_id){
    value2set <- estimates_indifferent$calibrated[estimates_indifferent$basin_id == bad_basin]
    basins[(basins == bad_basin) & (!is.na(basins))] <- value2set
  }

  if (!sum(is.na(basins)) == length(basins)){
    basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
    basins_polygons <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)
    indifferent_basins_list[[cont_id]] <- basins_polygons
  }

  # #getting all calibrated gamma values
  # basins <- as.vector(watergap3data::unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
  # basins[(!basins %in% red_y$ID) & (!is.na(basins))] <- NA
  # for (bad_basin in red_y$ID){
  #   value2set <- red_y$mean_gamma[red_y$ID == bad_basin]
  #   basins[(basins == bad_basin) & (!is.na(basins))] <- value2set
  # }
  #
  # if (!sum(is.na(basins)) == length(basins)){
  #   basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
  #   basins_polygons <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)
  #   gamma_basins_list[[cont_id]] <- basins_polygons
  # }


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
  scale_fill_manual(values=rev(hcl.colors(12, "Spectral"))[3:12]) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

ggsave(file.path("./plots", "review_bad_basins_map.png"),
       width=24, height=16, units="cm", dpi=300)

par(mfrow=c(1,1))
par(bg=NA)
hist(overlapping_map$n, xlab="number of methods", ylab="basins with KGE \u2264 0.2", main="")
dev.copy(png, file.path("./plots", "review_kge_map_hist.png"),
         width=12, height=12, units="cm", res=300)
dev.off()



indifferent_basins_without_null <- remove_null_from_list(indifferent_basins_list)
indifferent_basins_without_null_world <- do.call("rbind", indifferent_basins_without_null)
#gamma_basins_world <- do.call("rbind", gamma_basins_list)
ggplot() +
  ggspatial::geom_sf() +
  ggspatial::geom_sf(data=sf::st_as_sf(wmap_robin), fill=NA, col="black", size=0.25) +
  ggspatial::geom_sf(data=sf::st_as_sf(ungauged_basins_world), fill=rev(hcl.colors(12, "Viridis"))[1]) +
  ggspatial::geom_sf(data=sf::st_as_sf(gauged_basins_world), fill="lightgrey") +
  ggspatial::geom_sf(data=sf::st_as_sf(indifferent_basins_without_null_world),
                     aes(fill=layer)) +
  scale_fill_continuous("viridis") +
  #scale_fill_manual(values=rev(hcl.colors(12, "Viridis"))[3:12]) +
  theme_bw() +
  theme(legend.position="right",
        legend.title = element_blank())

ggsave(file.path("./plots", "review_indifferent_basins_map.png"),
       width=24, height=16, units="cm", dpi=300)

weird_ids <- estimates_indifferent$basin_id[estimates_indifferent$calibrated != 5]
params <- red_x_orig[red_y$ID %in% weird_ids,]
ids <- red_y$ID[red_y$ID %in% weird_ids]
plot(params$mean_temp, params$mean_op_water, xlim = c(-10, 30))
text(params$mean_temp, params$mean_op_water,
     ids, cex=0.6, pos=4, col="black")


ranks <- kge_all %>%
  filter(., !runtype %in% c("CAL", "DONOR")) %>%
  group_by(station_id) %>%
  mutate(rank = dense_rank(KGE)) %>%
  group_by(runtype) %>%
  summarize(sum(rank)) %>%
  as.data.frame()

ranks$normalized <- (ranks$`sum(rank)`-  min(ranks$`sum(rank)`)) / (max(ranks$`sum(rank)`) - min(ranks$`sum(rank)`))

# Barplot
ggplot(ranks, aes(x=runtype , y=normalized)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x ="", y = "normalized ranks") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggsave(file.path("./plots", "review_normalized_ranks.png"),
       width=10, height=10, units="cm", dpi=300)

