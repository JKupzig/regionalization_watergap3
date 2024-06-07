
library(raster)
library(ggplot2)

SUM_RUNOFF_PATH <- "./data/sum_runoff_global.tif"
SD_GAMMA_PATH <- "./data/CV_GAMMA_global.tif"
CV_RUNOFF_PATH <- "./data/cv_runoff_global.tif"

sum_runoff <- raster::raster(SUM_RUNOFF_PATH)
sd_gamma <- raster::raster(SD_GAMMA_PATH)
cv_runoff <- raster::raster(CV_RUNOFF_PATH)

mask_no_change_in_gamma <- sd_gamma >= 0.5
no_na <- !is.na(sum_runoff) &
  !is.na(sd_gamma) &
  !is.na(cv_runoff)

runoff_no_change_in_gamma <- sum_runoff[mask_no_change_in_gamma & no_na]
no_change_in_gamma <- sd_gamma[mask_no_change_in_gamma & no_na]
cv_runoff_no_change_in_gamma <- cv_runoff[mask_no_change_in_gamma & no_na]

less_runoff <- runoff_no_change_in_gamma > 100000

cor(no_change_in_gamma[less_runoff], cv_runoff_no_change_in_gamma[less_runoff])

data <- data.frame(sum_runoff = log(runoff_no_change_in_gamma+1),
                   cv_gamma = no_change_in_gamma,
                   cv_runoff = cv_runoff_no_change_in_gamma)

data$groups_sum <- ceiling(data$sum_runoff)
data$groups_sum <- factor(data$groups_sum, levels=seq(0, max(data$groups_sum), 1))

data <- data[1:2000, ]
ggplot(acs_small, aes(x = edu, y = income, color = age)) +
  geom_jitter(width = .2)


ggplot(data, aes(x=cv_runoff,
                 y=cv_gamma,
                 col=sum_runoff))+
  geom_point()


boxplot(cv_runoff_no_change_in_gamma ~ no_change_in_gamma)
