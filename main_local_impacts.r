#setwd(r"(C:\Users\jenny\MyProject_sciebo\_Nina\Regionalization\R\data_availability\regionalization_watergap3)")
# set you own working directory (position of run.r) here

# Created subplots for Figure 6 and 7

rm(list=ls())

plot_lines <- function(results, ylabel, xlabel, name, legend=NULL){
  
  TARGETFOLDER <- "./plots"
  
  if (is.null(legend)){
    legend = "topleft"
  }
  LWD = 1.5
  CEX = 1.5
  year_sp<- results$SP
  year_new <- results$NEW
  year_wg2 <- results$WG2
  y_min <- min(c(year_sp, year_new, year_wg2)) - abs(min(c(year_sp, year_new, year_wg2)))*0.11
  y_max <- max(c(year_sp, year_new, year_wg2)) + abs(max(c(year_sp, year_new, year_wg2)))*0.11
  png(file = file.path(TARGETFOLDER, name), width = 500, height = 400, units = "px") #, res=300 
  par(mar=c(5,6,4,1)+.1)
  plot(year_sp, type = "o", col = "firebrick", 
       xlab = xlabel, ylab = ylabel,
       ylim=c(y_min, y_max),
       lwd = LWD, cex=CEX, cex.lab=CEX, cex.axis=CEX)
  lines(year_wg2, type = "o", col = "blue", lwd = LWD, cex=CEX)
  lines(year_new, type = "o", col = "cornflowerblue", lwd = LWD, cex=CEX)
  legend(legend, legend=c("SP", "MLR tuned 'p+cl'", "WG2"),
         col=c("firebrick", "cornflowerblue", "blue"), lty=1, cex=CEX)
  
  dev.off()
}

folder2use <- "./data/LocalData"

# Figure 6
gw_india <- read.table(file.path(folder2use, "india_gw_1989.txt"))
plot_lines(gw_india, "Groundwater Storage (mm)", 
           sprintf("Month (%d)", 1989), 
           sprintf("Figure_6_india_gw_%d.png", 1989))
aet_india <- read.table(file.path(folder2use, "india_aet_1989.txt"))
plot_lines(aet_india, "Actual Evapotranspiration (mm)", 
           sprintf("Month (%d)", 1989), 
           sprintf("Figure_6_india_aet_%d.png", 1989))
soil_india <- read.table(file.path(folder2use, "india_soil_1989.txt"))
plot_lines(soil_india, "Soil Storage (mm)", 
           sprintf("Month (%d)", 1989), 
           sprintf("Figure_6_india_soil_%d.png", 1989))

# Figure 7
tiber <- read.table(file.path(folder2use, "tiber_2010.txt"))
plot_lines(tiber, "Discharge (m3/s)",
           sprintf("Month (%d)", 2010),
           sprintf("Figure_7_tiber_%d.png", 2010), 
           "topleft")
ebro <- read.table(file.path(folder2use, "ebro_2010.txt"))
plot_lines(ebro, "Discharge (m3/s)",
           sprintf("Month (%d)", 2010),
           sprintf("Figure_7_ebro_%d.png", 2010), 
           "topright")
rio_negro <- read.table(file.path(folder2use, "rio_negro_2010.txt"))
plot_lines(rio_negro, "Discharge (m3/s)",
           sprintf("Month (%d)", 2010),
           sprintf("Figure_7_rio_negro_%d.png", 2010), 
           "topleft")

  