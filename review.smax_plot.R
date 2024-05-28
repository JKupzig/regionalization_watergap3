
library(watergap3data)

root <- "./data"

global_smax <- watergap3data::unf.unf2globalraster(
  af_path=file.path(root, "G_Smax_af.UNF0"),
  as_path=file.path(root, "G_Smax_as.UNF0"),
  au_path=file.path(root, "G_Smax_au.UNF0"),
  eu_path=file.path(root, "G_Smax_eu.UNF0"),
  na_path=file.path(root, "G_Smax_na.UNF0"),
  sa_path=file.path(root, "G_Smax_sa.UNF0")
)


raster::plot(global_smax)
raster::writeRaster(global_smax, file.path(root, "global_smax.tiff"))

# rest is done in QGIS
