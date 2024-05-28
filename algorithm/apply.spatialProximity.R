
source("./algorithm/mae.r")

apply.spatialProximity <- function(ind, distanceMatrix, y, k=1,mod=F, tuningPars=c(1.18,3.48)){

  getMeanGamma <- function(matrix2look, basinList, id){

    i <- 1
    id2use <- basinList[id]
    row2use <- which(rownames(matrix2look) == id2use)
    matrixEntry <- matrix2look[row2use,]
    if (sort(matrixEntry, decreasing=F)[1] == 0) { i=2; k=k+1}
    minDistances <- sort(matrixEntry, decreasing=F)[i:k]
    id4gamma <- which(names(matrixEntry) %in% names(minDistances) )
    gamma2use <- mean(data.training.gamma[id4gamma])
    return(gamma2use)
  }

  data.training <- distanceMatrix[,ind==1] #c(3,6,7,8,11,13,16,17,19,20,21,22,23,24,25)
  basins <- colnames(distanceMatrix)
  rownames(data.training) <- basins
  basinsCal <- colnames(distanceMatrix[,ind==1])
  basinsVal <- colnames(distanceMatrix[,ind==2]) #c(3,6,7,8,11,13,16,17,19,20,21,22,23,24,25)

  data.training.gamma <- y[ind==1]
  data.test.gamma <- y[ind==2]

  estimatedGammaVec_cal <- sapply(1:length(basinsCal), function(x) { getMeanGamma(data.training , basinsCal, x) })
  estimatedGammaVec_val <- sapply(1:length(basinsVal), function(x) { getMeanGamma(data.training , basinsVal, x) })

  if (mod==T){
    estimatedGammaVec_cal <- ifelse(as.numeric(estimatedGammaVec_cal)>tuningPars[2], 5,
                         ifelse(as.numeric(estimatedGammaVec_cal)<tuningPars[1], 0.1, as.numeric(estimatedGammaVec_cal)))
    estimatedGammaVec_val <- ifelse(as.numeric(estimatedGammaVec_val)>tuningPars[2], 5,
                         ifelse(as.numeric(estimatedGammaVec_val)<tuningPars[1], 0.1, as.numeric(estimatedGammaVec_val)))
  }

  mae_cal <- mae(obs=data.training.gamma, sim=estimatedGammaVec_cal)
  mae_val <- mae(obs=data.test.gamma, sim=estimatedGammaVec_val)
  
  y_estimated <- y
  y_estimated[ind==1] <- estimatedGammaVec_cal
  y_estimated[ind==2] <- estimatedGammaVec_val
  df_result = data.frame(list("y_given"=y, "y_estimated"=y_estimated, "ind"=ind))

  list2return = list("mae_cal"=mae_cal, "mae_val"=mae_val, "result"=df_result)

  return(list2return)
}

