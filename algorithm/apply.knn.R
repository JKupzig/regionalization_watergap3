source("./algorithm/mae.r")

apply.knn <- function(ind,  x, y, yGamma, xcols=c(10,16,17,25,21), n=3, wGroups=F){

  data.training <- x[ind==1, xcols]
  data.test     <- x[ind==2, xcols]

  trainingGamma <- y[ind==1] #
  testGamma     <- y[ind==2]

  yGamma_cal <- yGamma[ind==1]
  yGamma_val <- yGamma[ind==2]

  gamma_cal <- class::knn(train = data.training, test = data.training, cl = trainingGamma, k=10)
  gamma_val <- class::knn(train = data.training, test = data.test, cl = trainingGamma, k=10)

  if (wGroups==T){
    gamma_cal2 <- getGamma(gamma_cal, n)
    gamma_val2 <- getGamma(gamma_val, n)
  } else {
    gamma_cal2 <- ifelse(as.numeric(gamma_cal)>3.48, 5,
                         ifelse(as.numeric(gamma_cal)<1.18, 0.1, as.numeric(gamma_cal)))
    gamma_val2 <- ifelse(as.numeric(gamma_val)>3.48, 5,
                         ifelse(as.numeric(gamma_val)<1.18, 0.1, as.numeric(gamma_val)))
  }
  mae_cal <- mae(obs=yGamma_cal, sim=gamma_cal2)
  mae_val <- mae(obs=yGamma_val, sim=gamma_val2)

  df_cal = data.frame(obs=yGamma_cal, sim=gamma_cal2)
  df_val = data.frame(obs=yGamma_val, sim=gamma_val2)

  list2return = list("mae_cal"=mae_cal, "mae_val"=mae_val, "df_cal"=df_cal, "df_val"=df_val)

  return(list2return)
}
