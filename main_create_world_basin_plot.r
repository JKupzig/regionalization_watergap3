setwd(r"(C:\Users\jenny\MyProject_sciebo\_Nina\Regionalization\R\data_availability\regionalization_watergap3)")
# set you own working directory (position of run.r) here

# prodcues Figure 1a

library(ggplot2)
library(tidyr)
library(dplyr)

library(watergap3data)

source("./helper_functions.r")

MIN_SIZE <- 5000
MIN_QUALITY <- 0.2
BASEMAP <- "C:/Users/jenny/MyProject_sciebo/SensitivityAnalysis/ne_110m_land"
CALIB_FOLDER <- r"(C:/Users/jenny/MyProject_sciebo/_Nina/Regionalization/_Data/2nd_Calibration)"

data_folder <- "./data"
reducer_quality <- create_subset(MIN_QUALITY, NULL)
reducer_all <- create_subset(MIN_QUALITY, MIN_SIZE)

quality <- readRDS(file.path(data_folder, "NEW_quality_monthly_bias.rds"))
id_order <- readRDS(file.path(data_folder, "NEW_IDs.rds"))
quality <- quality[order(match(quality$V1, id_order)), ]

basins4calibration <-  quality[, 1]
basins4regression <- basins4calibration[reducer_all]
basins4regression_quality <- basins4calibration[reducer_quality]

#reading base map
wmap <- rgdal::readOGR(dsn=BASEMAP, layer=basename(BASEMAP))
wmap_robin <- sp::spTransform(wmap, sp::CRS("+proj=robin"))


j <- 1
used_basins <- list()
calibrated_basins <- list()
ungauged_basins <- list()
calibrated_basins_too_small<- list()
for (cont in c("sa", "af", "as", "au", "eu", "na")) {

  rlog::log_info(sprintf("starting continent %s", cont))

  RasterTempl = watergap3data::GCRC_list[[cont]]
  crs2use <- raster::crs(RasterTempl)

  name_basins <- sprintf("G_BASCALIB_GRDC_%s.UNF0", cont)

  #getting only used catchments
  basins <- as.vector(unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
  basins[!basins %in% basins4regression] <- NA
  basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
  basins_polygons <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)

  #getting calibrated catchments withput low quality
  basins <- as.vector(unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
  basins[!basins %in% basins4regression_quality] <- NA
  basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
  basins_polygons_2 <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)

  #getting all calibrated catchments
  basins <- as.vector(unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
  basins[!basins %in% basins4calibration] <- NA
  basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
  basins_polygons_1 <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)

  #getting spatial units used for regionalization of ungauged regions
  table2use  <- read.csv(file.path(CALIB_FOLDER, paste0("mother_", cont, ".csv")), sep = ",")
  table2use  <- table2use[order(table2use$arcid), ]
  mask2use   <- -table2use$bas0_id
  mask2use   <- unf.sortARCID(mask2use, cont)

  mask2use[!is.na(basins)] <- NA #"deleting regions that are already in testdata
  mask2use_raster <- watergap3data::unf.vec2raster(as.vector(mask2use), 1, cont)
  mask2use_polygon <- raster::rasterToPolygons(mask2use_raster, na.rm=TRUE, dissolve=TRUE)

  #saving everything for global plot
  used_basins[[j]] <- basins_polygons
  calibrated_basins[[j]] <- basins_polygons_1
  calibrated_basins_too_small[[j]] <- basins_polygons_2
  ungauged_basins[[j]] <- mask2use_polygon
  j <- j + 1
}

#merging everything
used_basins_world <- do.call("rbind", used_basins)
calibrated_basins_world <- do.call("rbind", calibrated_basins)
ungauged_basins_world <- do.call("rbind", ungauged_basins)
to_small_basins_world <- do.call("rbind", calibrated_basins_too_small)


#plotting everything
ggplot() +
  ggspatial::geom_sf() +
  ggspatial::geom_sf(data= sf::st_as_sf(wmap_robin), fill=NA, col="black", size=0.25) +
  ggspatial::geom_sf(data=sf::st_as_sf(calibrated_basins_world), mapping=aes(fill="not used")) +
  ggspatial::geom_sf(data=sf::st_as_sf(to_small_basins_world), mapping=aes(fill="not used due to size")) +
  ggspatial::geom_sf(data=sf::st_as_sf(used_basins_world), mapping=aes(fill="used")) +
  scale_fill_manual(nam = "", values = c("not used" = "grey",
                                      "not used due to size" = "brown",
                                      "used" = "cornflowerblue"),
                    labels=c("basins with too low quality", "basins too small","used basins")) +
  coord_sf(expand = FALSE, xlim = c(-12000372.7, 15035574), ylim = c(-8235574, 8235574)) +
  theme_bw() +
  theme(legend.position = c(0.5, 0.11),
        legend.key.size = unit(0.3, 'cm'),
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.direction = "horizontal",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

ggsave("./plots/Figure_1a_worldplot.png", plot = last_plot(), device = "png",
       width = 22, height = 10, units = "cm", dpi = 300)
