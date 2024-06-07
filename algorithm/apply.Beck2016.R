#not using eny infromation about parameters

source("./algorithm/mae.r")

run.Beck2016 <- function(
    ind,
    x,
    y,
    n_ensemble,
    tuningPars,
    robustness_in_train = FALSE){

  upper <- tuningPars[2]
  lower <- tuningPars[1]

  getMeanGamma <- function(matrixEntry, n_ensemble, gamma.training, HDes="off")
  { #getting calibrated value for nearest neighbour

    start = 1
    end = n_ensemble

    if (HDes=="on"){
      start = start + 1
      end = n_ensemble + 1
    }
    minDistances <- sort(matrixEntry, decreasing=F)[start:end]
    nearestNeighour <- which(matrixEntry %in% minDistances)
    estimatedGamma <- mean(gamma.training[nearestNeighour])

    return(estimatedGamma)
  }

  get_estimated_gamma <- function(
    distanceMatrix, n_ensemble, gamma.training, HDes="off"){

    distanceMatrixWeights <- apply(distanceMatrix, 1:2, sum)
    estimatedGammaVec <- apply(
      distanceMatrixWeights, 1, function(x) {
        getMeanGamma(x, n_ensemble, gamma.training, HDes)
        }
      )
    return(estimatedGammaVec)
  }

  data.training <- as.matrix(x[ind==1,])
  gamma.training <- y[ind==1]
  data.test <- as.matrix(x[ind==2, ])
  gamma.test <- y[ind==2]

  IQR.training <- apply(data.training, 2, function(x) { quantile(x, 0.75) - quantile(x, 0.25) })

  distanceMatrix <-  array(NA, dim=c(nrow(data.training),nrow(data.training), ncol(data.training)))
  for (i in 1:ncol(data.training)){
    for (y in 1:nrow(data.training)){
      distanceMatrix[y,,i] <- apply(as.matrix(data.training[,i]), 2, function(x) { (abs(x - data.training[y,i]))/IQR.training[i] })
    }
  }

  #setting diagonal entries with 0 derivation to 999 - e.g., catchment is estimated based on nearest catchment in group
  for (d in 1:ncol(data.training))
  {
    diag(distanceMatrix[,,d]) <- 999
  }

  #apply approach within gauged basins
  robustness = "off"
  if (robustness_in_train == TRUE)
  {
    robustness = "on"
  }
  gamma_cal <- get_estimated_gamma(distanceMatrix, n_ensemble, gamma.training, HDes=robustness)


  distanceMatrixCal <-  array(NA, dim=c(nrow(data.test), nrow(data.training), ncol(data.training)))
  for (i in 1:ncol(data.training)){
    for (y in 1:nrow(data.test)){
      distanceMatrixCal[y,,i] <- apply(as.matrix(data.training[,i]), 2, function(x) { (abs(x - data.test[y,i]))/IQR.training[i] })
    }
  }
  #apply approach for ungauged basins
  gamma_val <- get_estimated_gamma(distanceMatrixCal, n_ensemble, gamma.training, HDes="off")

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

apply.Beck2016 <- function(ind,
                           x,
                           y,
                           n_ensemble,
                           tuningPars=c(1.18,3.48)){


  precictions <- run.Beck2016(ind,x, y, n_ensemble, tuningPars)
  mae_cal <- mae(obs=precictions$gamma_cal_calibrated,
                 sim=precictions$gamma_cal_limited)
  mae_val <- mae(obs=precictions$gamma_val_calibrated,
                 sim=precictions$gamma_val_limited)

  mae_cal_t <- mae(obs=precictions$gamma_cal_calibrated,
                   sim=precictions$gamma_cal_tuned)
  mae_val_t <- mae(obs=precictions$gamma_val_calibrated,
                   sim=precictions$gamma_val_tuned)

  list2return = list("mae_cal"=mae_cal,
                     "mae_val"=mae_val,
                     "mae_cal_t"=mae_cal_t,
                     "mae_val_t"=mae_val_t)
  return(list2return)
}
