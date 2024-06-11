source("./algorithm/mae.r")

run.RandomForest <- function(ind,
                               x,
                               y,
                               tuningPars=c(1.17, 2.64),
                               NTrees=150,
                               mTRY=2){

  upper <- tuningPars[2]
  lower <- tuningPars[1]

  data.training <- data.frame(x[ind==1,])
  data.test     <- data.frame(x[ind==2,])

  data.training$result <- (y[ind==1])

  data.rf <- randomForest::randomForest(result~ .,
                                        data=data.training,
                                        importance=F,
                                        proximity=F,
                                        ntree=NTrees,
                                        mtry=mTRY)


  gamma_cal <- stats::predict(data.rf, data.training)
  gamma_val <- stats::predict(data.rf, data.test)

  gamma_cal_limited <- limit_gamma_to_be_valid(gamma_cal, upper_limit=5, lower_limit=0.1)
  gamma_val_limited <- limit_gamma_to_be_valid(gamma_val, upper_limit=5, lower_limit=0.1)

  gamma_cal_tuned <- limit_gamma_to_be_valid(gamma_cal, upper_limit=upper, lower_limit=lower)
  gamma_val_tuned <- limit_gamma_to_be_valid(gamma_val, upper_limit=upper, lower_limit=lower)

  return(list(gamma_cal_limited=gamma_cal_limited,
              gamma_val_limited=gamma_val_limited,
              gamma_cal_tuned=gamma_cal_tuned,
              gamma_val_tuned=gamma_val_tuned,
              gamma_cal_calibrated=y[ind==1],
              gamma_val_calibrated=y[ind==2]))

}

apply.RandomForest <- function(ind,
                               x,
                               y,
                               tuningPars=c(1.17, 2.64),
                               NTrees=150,
                               mTRY=2){
  #nnce explanation video: https://www.youtube.com/watch?v=nxFG5xdpDto


  prediction <- run.RandomForest(ind=ind,
                                 x=x,
                                 y=y,
                                 tuningPars=tuningPars,
                                 NTrees=NTrees,
                                 mTRY=mTRY)

  mae_cal <- logmae(obs=prediction$gamma_cal_calibrated,
                 sim=prediction$gamma_cal_limited)
  mae_val <- logmae(obs=prediction$gamma_val_calibrated,
                 sim=prediction$gamma_val_limited)

  mae_cal_t <- logmae(obs=prediction$gamma_cal_calibrated,
                   sim=prediction$gamma_cal_tuned)
  mae_val_t <- logmae(obs=prediction$gamma_val_calibrated,
                   sim=prediction$gamma_val_tuned)

  list2return = list("mae_cal"=mae_cal,
                     "mae_val"=mae_val,
                     "mae_cal_t"=mae_cal_t,
                     "mae_val_t"=mae_val_t)

  return(list2return)
}


