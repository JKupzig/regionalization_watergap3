#not using eny infromation about parameters

source("./algorithm/mae.r")

apply.Beck2016 <- function(x, gamma2use, ind, n_ensemble, mod=F, tuningPars=c(1.18,3.48)){

  calcMAE <- function(distanceMatrix, gammaVals){

    getMeanGamma <- function(matrixEntry)
      { #getting calibrated value for nearest neighbour
      minDistances <- sort(matrixEntry, decreasing=F)[1:n_ensemble]
      nearestNeighour <- which(matrixEntry %in% minDistances)
      minDistances <- matrixEntry[nearestNeighour]
      orderDistance <- order(minDistances)
      estimatedGamma <- mean(gamma.training[nearestNeighour])
      if (mod==T)
        {
        estimatedGamma <-  ifelse(estimatedGamma>tuningPars[2], 5,
                                  ifelse(estimatedGamma<tuningPars[1], 0.1, estimatedGamma))
      }
      return(estimatedGamma)
    }

    distanceMatrixWeights <- apply(distanceMatrix, 1:2, sum)
    estimatedGammaVec <- apply(distanceMatrixWeights, 1, function(x) { getMeanGamma(x) })
    calc_mae <- mae(obs=gammaVals, sim=as.vector(estimatedGammaVec))
    return(calc_mae)
  }


  data.training <- as.matrix(x[ind==1,])
  gamma.training <- gamma2use[ind==1]
  data.test <- as.matrix(x[ind==2, ])
  gamma.test <- gamma2use[ind==2]

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
  MAE_cal <- calcMAE(distanceMatrix, gamma.training)

  distanceMatrixCal <-  array(NA, dim=c(nrow(data.test), nrow(data.training), ncol(data.training)))
  for (i in 1:ncol(data.training)){
    for (y in 1:nrow(data.test)){
      distanceMatrixCal[y,,i] <- apply(as.matrix(data.training[,i]), 2, function(x) { (abs(x - data.test[y,i]))/IQR.training[i] })
    }
  }
  #apply approach for ungauged basins
  MAE_val <- calcMAE(distanceMatrixCal, gamma.test)

  list2return = list("mae_cal"=MAE_cal, "mae_val"=MAE_val)

  return(list2return)
}
