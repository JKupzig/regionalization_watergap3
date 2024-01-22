source("./algorithm/mae.r")

apply.RandomForest <- function(ind, x, y, mod=F, tuningPars=c(1.17, 2.64),
                               NTrees=150, mTRY=2){
  #nnce explanation video: https://www.youtube.com/watch?v=nxFG5xdpDto

  data.training <- data.frame(x[ind==1,])
  data.test     <- data.frame(x[ind==2,])

  data.training$result <- (y[ind==1])

  data.rf <- randomForest::randomForest(result~ .,
                                        data=data.training,
                                        importance=F,
                                        proximity=F,
                                        ntree=NTrees,
                                        mtry=mTRY)


  #Validation
  gamma_cal <- stats::predict(data.rf, data.training)
  gamma_val <- stats::predict(data.rf, data.test)

  if (mod==F)
    {
    gamma_cal <- ifelse(gamma_cal > 5, 5,
                         ifelse(gamma_cal < 0.1, 0.1, gamma_cal))
    gamma_val <- ifelse(gamma_val > 5, 5,
                         ifelse(gamma_val < 0.1, 0.1, gamma_val))
  } else {
    gamma_cal <- ifelse(as.numeric(gamma_cal)>=tuningPars[2], 5,
                         ifelse(as.numeric(gamma_cal)<=tuningPars[1], 0.1, as.numeric(gamma_cal)))
    gamma_val <- ifelse(as.numeric(gamma_val)>=tuningPars[2], 5,
                         ifelse(as.numeric(gamma_val)<=tuningPars[1], 0.1, as.numeric(gamma_val)))
  }

  mae_cal <- mae(obs=y[ind==1], sim=gamma_cal)
  mae_val <- mae(obs=y[ind==2], sim=gamma_val)

  df_cal = data.frame(obs=y[ind==1], sim=gamma_cal)
  df_val = data.frame(obs=y[ind==2], sim=gamma_val)

  list2return = list("mae_cal"=mae_cal, "mae_val"=mae_val, "df_cal"=df_cal, "df_val"=df_val)

  return(list2return)
}


