
source("./algorithm/mae.r")

run.spatialProximity <- function(
    ind,
    distances,
    gamma,
    k=1,
    robustness_in_train = FALSE){

  jump = 1
  if (robustness_in_train == TRUE){
    jump = 2
  }

  rownames(distances) <- colnames(distances)
  distances2use <- distances[,ind==1]
  gamma_gauged_basins <- gamma[ind==1]

  all_basins <- colnames(distances)
  ungauged_basins <- colnames(distances[,ind==2])

  predicted_values <- c()
  for (basin in all_basins){
    distances_to_basin <- distances2use[which(rownames(distances2use) == basin),]

    use_nearest = 1 # to create robustness test set to 2 (skipping same & nearest basin)
    end = k
    if (sort(distances_to_basin, decreasing=F)[1] == 0) {
      use_nearest = use_nearest + jump # to enable "training"
      end = end + jump
    }

    nearest_basins <- sort(distances_to_basin, decreasing=F)[use_nearest:end]
    nearest_basins_ids <- which(rownames(distances2use) %in% names(nearest_basins) )
    predicted_gamma <- mean(gamma[nearest_basins_ids])
    predicted_values <- c(predicted_values, predicted_gamma)
  }

  return(data.frame(basins=all_basins,
                    predicted_gamma=predicted_values,
                    calibrated_gamma=gamma,
                    mapping=ind))
}

apply.spatialProximity <- function(
    ind,
    x,
    y,
    k=1,
    tuningPars=c(1.18,3.48)){

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

  data.training <- x[,ind==1]
  basins <- colnames(x)
  rownames(data.training) <- basins
  basinsCal <- colnames(x[,ind==1])
  basinsVal <- colnames(x[,ind==2]) #c(3,6,7,8,11,13,16,17,19,20,21,22,23,24,25)

  data.training.gamma <- y[ind==1]
  data.test.gamma <- y[ind==2]

  gamma_cal <- sapply(1:length(basinsCal), function(x) { getMeanGamma(data.training , basinsCal, x) })
  gamma_val <- sapply(1:length(basinsVal), function(x) { getMeanGamma(data.training , basinsVal, x) })

  gamma_cal_limited <- limit_gamma_to_be_valid(gamma_cal, upper_limit=5, lower_limit=0.1)
  gamma_val_limited <- limit_gamma_to_be_valid(gamma_val, upper_limit=5, lower_limit=0.1)

  gamma_cal_tuned <- limit_gamma_to_be_valid(gamma_cal, upper_limit=upper, lower_limit=lower)
  gamma_val_tuned <- limit_gamma_to_be_valid(gamma_val, upper_limit=upper, lower_limit=lower)

  mae_cal <- mae(obs=data.training.gamma, sim=gamma_cal_limited)
  mae_val <- mae(obs=data.test.gamma, sim=gamma_val_limited)

  mae_cal_t <- mae(obs=data.training.gamma, sim=gamma_cal_tuned)
  mae_val_t <- mae(obs=data.test.gamma, sim=gamma_val_tuned)

  list2return = list("mae_cal"=mae_cal,
                     "mae_val"=mae_val,
                     "mae_cal_t"=mae_cal_t,
                     "mae_val_t"=mae_val_t)

  return(list2return)
}

