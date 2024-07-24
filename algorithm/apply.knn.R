source("./algorithm/mae.r")

run.knn <- function(
    ind,
    x,
    y,
    tuningPars,
    robustness_in_train=FALSE,
    shuffle=NULL){

  lower = tuningPars[1]
  upper = tuningPars[2]

  # sub-sampling
  data.training <- x[ind==1,]
  data.test <- x[ind==2,]

  #shuffling testing if defined
  if (length(shuffle) == 1){
    data.test[, shuffle] <- sample(data.test[,shuffle])
  }

  gamma.training <- y[ind==1]
  gamma.test <- y[ind==2]

  # normalization
  normalizeMinMax <- function(x) {
    num <- x - min(x)
    denom <- max(x) - min(x)
    return (num/denom)
  }


  for (column in 1:ncol(data.training)){
    data.test[,column] <-  (data.test[,column]-min(data.training[,column])) / (max(data.training[,column]-min(data.training[,column])))
  }
  data.training <- apply(data.training, 2, normalizeMinMax)

  # clustering
  dummy_cluster <- as.integer(seq(1, nrow(data.training), 1))

  gamma_cal <- c()
  for (basin in 1:nrow(data.training)){
    data.training_without_basin <- data.training[-c(basin),]
    gamma.training_without_basin <- gamma.training[-c(basin)]
    dummy_cluster_without_basin <- dummy_cluster[-c(basin)]
    basin_info <- data.training[c(basin),]
    level_cal <- class::knn(train = data.training_without_basin,
                             test = basin_info,
                             cl = dummy_cluster_without_basin, k=1)

    if (robustness_in_train == TRUE)
    {
      # Approach for robustness after Lebecherel et al. (2016) HDes
      data.training_without_nearest_basin <- data.training[-c(basin, as.integer(as.character(level_cal))),]
      gamma.training_without_nearest_basin <- gamma.training[-c(basin,as.integer(as.character(level_cal)))]
      dummy_cluster_without_nearest_basin <- dummy_cluster[-c(basin, as.integer(as.character(level_cal)))]
      basin_info_without_nearest_basin <- data.training[c(basin),]
      level_cal <- class::knn(train = data.training_without_nearest_basin,
                              test = basin_info_without_nearest_basin,
                              cl = dummy_cluster_without_nearest_basin, k=1)
    }
    gamma_nearest_basin <- gamma.training[as.integer(as.character(level_cal))]
    gamma_cal <- c(gamma_cal, gamma_nearest_basin)
  }

  levels_val <- class::knn(train = data.training, test = data.test, cl = dummy_cluster, k=1)

  # evaluation
  gamma_val <- gamma.training[as.integer(levels_val)]

  gamma_cal_limited <- limit_gamma_to_be_valid(gamma_cal, upper_limit=5, lower_limit=0.1)
  gamma_val_limited <- limit_gamma_to_be_valid(gamma_val, upper_limit=5, lower_limit=0.1)

  gamma_cal_tuned <- limit_gamma_to_be_valid(gamma_cal, upper_limit=upper, lower_limit=lower)
  gamma_val_tuned <- limit_gamma_to_be_valid(gamma_val, upper_limit=upper, lower_limit=lower)

  return(list(gamma_cal_limited=gamma_cal_limited,
              gamma_val_limited=gamma_val_limited,
              gamma_cal_tuned=gamma_cal_tuned,
              gamma_val_tuned=gamma_val_tuned,
              gamma_cal_calibrated=gamma.training,
              gamma_val_calibrated=gamma.test))
}

apply.knn <- function(ind,
                      x,
                      y,
                      tuningPars=c(1.16, 3.5)){

  precictions <- run.knn(ind, x, y, tuningPars)

  mae_cal <- logmae(obs=precictions$gamma_cal_calibrated,
                 sim=precictions$gamma_cal_limited)
  mae_val <- logmae(obs=precictions$gamma_val_calibrated,
                 sim=precictions$gamma_val_limited)

  mae_cal_t <- logmae(obs=precictions$gamma_cal_calibrated,
                   sim=precictions$gamma_cal_tuned)
  mae_val_t <- logmae(obs=precictions$gamma_val_calibrated,
                   sim=precictions$gamma_val_tuned)

  list2return = list("mae_cal"=mae_cal,
                     "mae_val"=mae_val,
                     "mae_cal_t"=mae_cal_t,
                     "mae_val_t"=mae_val_t)


  return(list2return)
}
