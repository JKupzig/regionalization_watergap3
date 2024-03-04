# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# Created plota for Figure 5a-c

rm(list=ls())

ROOT <- "./data"
TARGETFOLDER <- "./plots"
gamma_worldwide <- raster::brick(file.path(ROOT, "gamma_worldwide.grd"))

png(file.path(TARGETFOLDER, "Figure_5a_c.png"),
    width=55, height=15,
    res=300, pointsize = 6, units="cm")

par(mfrow=c(1,3))
par(mar = c(10, 2, 2, 2))

color2use <-"GnBu"
my_window <- raster::extent(-170, 170, -60, 80)
raster::plot(my_window, col=NA, ylab = "",xlab ="",
             main="gamma - wg2 regionalization")

raster::plot(gamma_worldwide$WG2,
             breaks = c(0., 0.1, 0.2, 1, 2, 4, 4.9, 5),
             col = RColorBrewer::brewer.pal(7,color2use),
             legend=F,
             cex.lab=0.2,
             cex.axis = 0.6,
             box=F, axis=F,
             add =T)
par(xpd=T)
raster::plot(my_window, col=NA, ylab = "",xlab ="",
             main="gamma - new regionalization")
raster::plot(gamma_worldwide$MLR,
             breaks = c(0., 0.1, 0.2, 1, 2, 4, 4.9, 5),
             col = RColorBrewer::brewer.pal(7,color2use),
             legend=F,
             cex.lab=0.2,
             cex.axis = 0.6,
             box=F, axis=F,
             add=T)

par(xpd=T)
raster::plot(my_window, col=NA, ylab = "",xlab ="",
             main="gamma - sp regionalization")
raster::plot(gamma_worldwide$SP,
             breaks = c(0., 0.1, 0.2, 1, 2, 4, 4.9, 5),
             col = RColorBrewer::brewer.pal(7,color2use),
             legend=F,
             cex.lab=0.2,
             cex.axis = 0.6,
             box=F, axis=F,
             add=T)

par(xpd=NA)
legend(
  -200,-75 , #-800 -95
  legend=c("0.1", "0.1 to 0.2", "0.2 to 1", "1 to 2", "2 to 4", "4 to 4.9", "5.0"),
  fill = RColorBrewer::brewer.pal(7,color2use),
  cex=1.3,
  border=NA,
  bty="n",
  ncol = 4,
  title = "gamma (-)"
)

dev.off()
