# setwd(r"(C:\Users\jenny\...")
# set you own working directory (position of run.r) here

# evaluates daily discharge of selected ungauged river systems regarding differences
# in mean annual percentiles and produces Figure 7a-d

###################################################################################################
# Functions
###################################################################################################

rm(list=ls())

library(watergap3data)
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(ggplot2)


# Einlesen discharge_station_data_daily.out (Model-Output)
# @param file_path filepath to daily .out file (model output)
# @return dataframe of daily data
txt.read_station_discharge_daily <- function(file_path){

  DEFAULT_LINES_TO_SKIP <- 2
  DAYS_IN_YEAR <- 365

  lines_to_skip <- DEFAULT_LINES_TO_SKIP
  first_lines = read_line_by_line(file_path, 3)
  if (startsWith(first_lines[3], "#")) {
    warm_up_years <- gsub('.+\\(([0-9]+)(?:-([0-9]+))?\\).*$', '\\1\\2', first_lines[3])
    lines_to_skip <- DEFAULT_LINES_TO_SKIP + DAYS_IN_YEAR*as.integer(warm_up_years) + as.integer(warm_up_years)
  }

  basin_names <-  strsplit(first_lines[2], "\t")[[1]]

  data <- read.csv(file_path,
                   skip=lines_to_skip,
                   sep="\t",
                   stringsAsFactors=F,
                   header=F, col.names=c(basin_names, "dummy"))

  data$date <- doy_to_date(data$Day, data$X..Year)
  data <- data[, -which(names(data) %in% c("Day","X..Year", "dummy"))]
  return(data)
}

quantile_df <- function(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  tibble(
    qs = quantile(x, probs, na.rm = TRUE),
    probs = probs*100
  )
}

###################################################################################################
# Constants
###################################################################################################


YEARS <- 1980:2016
OUTPUT_PATH <- "./data/LocalData/rivers_%s"
FOLDERS <- c("KNN", "SI", "MLR", "B2B", "SP")
DISCHARGE <- "STATION_DISCHARGE_DAILY_%s_%i.OUT"

X_LABEL <- expression('Discharge (m'^3*'s'^-1*')')
MULTIPLIER_UNITS <- 1000 * 1000 * 1000 / (24 * 3600)
DAILY <- TRUE

###################################################################################################
# Scripting
###################################################################################################

river_to_look <- list("tiber"=c("tiber", "eu", 16784, 1),
                      "pescara"=c("pescara", "eu", 3100, 2),
                      "ebro"=c("ebro", "eu", 56103, 1),
                      "tamar"=c("tamar", "au", 256, 3),
                      "rio_bravo"=c("rio_bravo", "na", 366252, 1),
                      "rio_chubut"=c("rio_chubut","sa", 93852, 1),
                      "rio_deseado"=c("rio_deseado", "sa", 22964, 1),
                      "rio_negro"=c("rio_negro", "sa", 112422, 1)
)

quantiles_to_use <- c(5, 25, 50, 75, 95)/100. #after Gudmundsson 2013
all_river_data <- NULL
for (river in names(river_to_look)){
  print(river)

  quantiles_overview <- NULL
  discharge_overview <- NULL

  accuracy_in_table <- as.numeric(river_to_look[[river]][4])
  basin_size <- as.numeric(river_to_look[[river]][3])
  cont <- river_to_look[[river]][2]
  river_name <- river_to_look[[river]][1]

  for (folder in FOLDERS){
    discharge_data <- NULL

    for (idx in c(1,2,3,4))+
      {
        folder_of_file <- sprintf(OUTPUT_PATH, cont)
        file_name <- sprintf(DISCHARGE, folder, idx)
        data <- watergap3data::txt.read_station_discharge_daily(file.path(folder_of_file, file_name))
        discharge_data <- rbind(discharge_data, data)
      }

    date_column <- which(names(discharge_data) == "date")

    monthly_timeseries <- discharge_data %>%
      mutate(identifier = format(date, "%Y-%m")) %>%
      mutate(value = get(river_name)) %>%
      select(c(date, identifier, value)) %>%
      group_by(identifier) %>%
      summarise(sum=sum(value)) %>%
      mutate(year =substr(identifier, 1,4)) %>%
      group_by(year) %>%
      reframe(quantile_df(sum)) %>%
      tidyr::pivot_wider(values_from=qs, names_from=c(probs)) %>%
      as.data.frame() %>%
      select(-c(year))


    annual_timeseries <- discharge_data %>%
      mutate(year = format(date, "%Y")) %>%
      mutate(value = get(river_name)) %>%
      select(c(year, value)) %>%
      group_by(year) %>%
      reframe(quantile_df(value)) %>%
      tidyr::pivot_wider(values_from=qs, names_from=c(probs)) %>%
      as.data.frame() %>%
      select(-c(year))

    mean_percentiles <- apply(monthly_timeseries, 2, mean) * MULTIPLIER_UNITS # / as.numeric(basin_size) # * IN_MM_PER_DAY / as.numeric(basin_size)
    sd_percentiles <- apply(monthly_timeseries, 2, sd) *MULTIPLIER_UNITS # / as.numeric(basin_size) #*  IN_MM_PER_DAY / as.numeric(basin_size)
    time_resolution <- "monthly"

    if (DAILY == TRUE)
    {
      mean_percentiles <- apply(annual_timeseries, 2, mean) * MULTIPLIER_UNITS # / as.numeric(basin_size) # * IN_MM_PER_DAY / as.numeric(basin_size)
      sd_percentiles <- apply(annual_timeseries, 2, sd) *MULTIPLIER_UNITS  # / as.numeric(basin_size) #*  IN_MM_PER_DAY / as.numeric(basin_size)
      time_resolution <- "daily"
    }

    quantiles <- data.frame(mean_percentiles=mean_percentiles,
                            sd_percentiles = sd_percentiles,
                            runtype=folder,
                            probs=quantiles_to_use*100)
    quantiles_overview <- rbind(quantiles_overview, quantiles)

    discharge_data$runtype <- folder
    discharge_overview <- rbind(discharge_overview, discharge_data)
  }


  my_table <- quantiles_overview %>%
    mutate(value = round(mean_percentiles, accuracy_in_table)) %>%
    rename("Perc." = probs) %>%
    group_by(runtype) %>%
    filter(Perc. %in% c(5, 50, 95)) %>%
    select(c(Perc., value, runtype)) %>%
    tidyr::pivot_wider(., values_from=value, names_from=runtype) %>%
    rename("knn" = KNN)


  quantiles_overview <- quantiles_overview[order(quantiles_overview$runtype), ]
  b2b <- quantiles_overview[quantiles_overview$runtype == "B2B", c(1, 2, 4)]
  all <- merge(quantiles_overview, b2b, by="probs", x.all=T)
  all <- all[order(all$runtype), ]
  normalized_mean <- (all$mean_percentiles.x - all$mean_percentiles.y) / all$mean_percentiles.y
  normalized_sd <- (all$sd_percentiles.x - all$sd_percentiles.y) / all$sd_percentiles.y

  river <- data.frame(normalized_mean=normalized_mean,
                      normalized_sd=normalized_sd,
                      probs=all$probs,
                      runtype=all$runtype,
                      river=river)
  all_river_data <- rbind(all_river_data, river)

  quantiles_overview$runtype <- as.character(quantiles_overview$runtype)
  if (identical(my_table[['SI']], my_table[['MLR']])) {
    quantiles_overview <- quantiles_overview[quantiles_overview$runtype != "MLR", ]
    quantiles_overview$runtype[quantiles_overview$runtype == "SI"] <- "SI, MLR"

    if (identical(my_table[['SI']], my_table[['KNN']])) {
      quantiles_overview <- quantiles_overview[quantiles_overview$runtype != "KNN", ]
      quantiles_overview$runtype[quantiles_overview$runtype == "SI, MLR"] <- "SI, MLR, knn"
    }
  }

  quantiles_overview$runtype[quantiles_overview$runtype == "KNN"] <- "knn"
  y_max <- max(quantiles_overview$mean_percentiles + quantiles_overview$sd_percentiles)
  ggplot(quantiles_overview, aes(x=probs, y=mean_percentiles, color=runtype)) +
    geom_line(position=position_dodge(3)) +
    geom_point(position=position_dodge(3))+
    scale_color_manual(name="", values=hcl.colors(length(FOLDERS)+1, "Viridis")) +
    theme_classic() +
    theme(legend.position = "bottom") +
    ylab(X_LABEL) +
    xlab("Percentile (%)") +
    scale_x_continuous(breaks=quantiles_to_use*100) +
    annotate(geom = "table", x = 5, y = y_max, label = list(my_table),
             vjust = 1, hjust = 0)

  ggsave(sprintf("./plots/Figure_7_%s_%s.png", river_name, time_resolution),
         width=11, height=8.5, units="cm", dpi=300)

}


shown_probs <- c(5, 25, 50, 75, 95)

all_river_data %>%
  filter(runtype != "B2B") %>%
  mutate(river = factor(river, levels=names(river_to_look),
                        labels=c("Tiber", "Pescara", "Ebro", "Tamar", "Rio Bravo", "Rio Deseado", "Rio Chubut", "Rio Negro"))) %>%
  mutate(runtype = factor(runtype, levels=c("KNN", "SI", "MLR", "SP"), labels=c("knn", "SI", "MLR", "SP"))) %>%
  filter(probs %in% shown_probs) %>%
  tidyr::pivot_longer(cols=c(normalized_mean)) %>%
  mutate(name = factor(name, levels=c("normalized_mean", "normalized_sd"),
                       ordered=T, labels=c(expression(mu), expression(sigma)))) %>%
  ggplot(., aes(x=probs, y=value, group=probs)) +
  geom_boxplot() +
  geom_jitter(aes(col=river, shape=runtype), alpha=0.8, width=4) +
  scale_colour_manual(name="", values=hcl.colors(length(river_to_look), "Spectral")) +
  ylim(-0.5,1.5) +
  ylab("Rel. differences to B2B (-)") +
  xlab("Percentile (%)") +
  theme_classic() +
  scale_x_continuous(breaks=shown_probs) +
  theme(legend.margin=margin(0,0,0,0),
        legend.title = element_blank())

ggsave(sprintf("./plots/Figure_7d_%s.png", time_resolution),
       width=11, height=8.5, units="cm", dpi=300)
