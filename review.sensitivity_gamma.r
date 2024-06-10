# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# Creates the comprehensible sensitivity analysis for gamma (used in the Appendix)

library(ggplot2)

target_folder <- "./plots"

hbv_formular <- function(precipitation, soil_saturation, gamma)
{
  outflow = precipitation * (soil_saturation ** gamma)
  return(outflow)
}

precipitation <- 100
soil_saturation <- c(0.01, 0.1, 0.2, 0.3, .4, .5, .6, .7, .8, .9, 0.95)
gamma <- seq(0.1, 10, 0.1)

row <- 0
outflow <- matrix(NA, nrow=length(soil_saturation), ncol = length(gamma))
ggplot_data <- c()
soil_info <- c()
gamma_info <- c()

for (saturation in soil_saturation)
{
  col <- 0
  row <- row + 1
  for (param in gamma)
  {
    col <- col + 1
    result <- hbv_formular(precipitation, saturation, param)
    outflow[row, col] <- result

    ggplot_data <- c(ggplot_data, result)
    soil_info <- c(soil_info, saturation)
    gamma_info <- c(gamma_info, param)

  }
}

png(file.path(target_folder, "review_line_plot_sa_gamma.png"),
    res=300, width=16, height=16, units="cm")

pal = colorRampPalette(c("navyblue", "firebrick"))
col <- 0
plot(soil_saturation*100, outflow[,1], ylim= c(0,100), type="l",
     ylab="runoff [% of precipitation]", xlab = "soil saturation [%]")

for (param in gamma)
{
  col <- col + 1
  lines(soil_saturation*100, outflow[,col],
        col=pal(length(gamma))[col])
}

legend("topleft", col=pal(2), lty=1,
       legend=c(round(range(gamma), 1)),
       title="gamma range")

dev.off()


data <- data.frame(list(
  "gamma_value"=gamma_info,
  "soil_saturation" = soil_info,
  "runoff" = ggplot_data
))

data$gamma <- ceiling(data$gamma_value - 0.05)
data$gamma <- factor(as.character(data$gamma), levels=as.character(seq(1,10,1)))
data$soil_saturation <- as.factor(data$soil_saturation * 100)

ggplot(data, aes(x=soil_saturation,
                 y=runoff,
                 group=gamma,
                 fill=gamma)) +
  scale_fill_manual(labels = c("0.1 to 1", "1 to 2", "2 to 3", "3 to 4", "4 to 5",
                                "5 to 6", "6 to 7", "7 to 8", "8 to 9", "9 to 10"),
                     values = list("1" = pal(10)[1],
                                   "2" = pal(10)[2],
                                   "3" = pal(10)[3],
                                   "4" = pal(10)[4],
                                   "5" = pal(10)[5],
                                   "6" = pal(10)[6],
                                   "7" = pal(10)[7],
                                   "8" = pal(10)[8],
                                   "9" = pal(10)[9],
                                   "10" = pal(10)[10]
                                   )) +
  geom_boxplot() +
  labs(y = "runoff (% of precipitation)", x = "soil saturation (%)") +
  facet_wrap(~soil_saturation, scale="free") +
  ylim(0, 100) +
  theme_bw()

ggsave(file.path(target_folder, "review_boxplot_sa_gamma.png"),
       width=16, height=16, units="cm", dpi=300)

min(data$runoff[data$soil_saturation == "70" & data$gamma_value > 5])
max(data$runoff[data$soil_saturation == "70" & data$gamma_value > 5])

min(data$runoff[data$soil_saturation == "70" & data$gamma_value <= 5 & data$gamma_value >= 1])
max(data$runoff[data$soil_saturation == "70" & data$gamma_value <= 5 & data$gamma_value >= 1])
