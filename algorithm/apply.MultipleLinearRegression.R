
source("./algorithm/mae.r")

apply.MultipleLinearRegression <- function(ind, x, y, mod=F, tuningPars=c(1.18,3.48), ln="off",
                                           return_regression = F){

  data.training <- x[ind==1, ] #c(3,6,7,8,11,13,16,17,19,20,21,22,23,24,25)
  data.test <- x[ind==2, ] #c(3,6,7,8,11,13,16,17,19,20,21,22,23,24,25)

  data.training$gamma <- y[ind==1] #
  data.test$gamma <- y[ind==2]

  if (ln=="on"){
    regr <- lm(log(data.training$gamma) ~ ., data.training)
    gamma_cal <- predict(regr, newdata=data.training)
    gamma_val <- predict(regr, newdata=data.test)
    gamma_cal <- exp(gamma_cal)
    gamma_val <- exp(gamma_val)
  } else {
    regr <- lm(data.training$gamma ~ ., data.training)
    gamma_cal <- predict(regr, newdata=data.training)
    gamma_val <- predict(regr, newdata=data.test)
  }


  if (mod==F){
    gamma_cal2 <- ifelse(gamma_cal>5, 5,
                         ifelse(gamma_cal<0.1, 0.1, gamma_cal))
    gamma_val2 <- ifelse(gamma_val>5, 5,
                         ifelse(gamma_val<0.1, 0.1, gamma_val))
  } else {
    gamma_cal2 <- ifelse(as.numeric(gamma_cal)>=tuningPars[2], 5,
                         ifelse(as.numeric(gamma_cal)<=tuningPars[1], 0.1, as.numeric(gamma_cal)))
    gamma_val2 <- ifelse(as.numeric(gamma_val)>=tuningPars[2], 5,
                         ifelse(as.numeric(gamma_val)<=tuningPars[1], 0.1, as.numeric(gamma_val)))
  }
  mae_cal <- mae(obs=data.training$gamma, sim=gamma_cal2)
  mae_val <- mae(obs=data.test$gamma, sim=gamma_val2)

  df_cal = data.frame(obs=data.training$gamma, sim=gamma_cal2)
  df_val = data.frame(obs=data.test$gamma, sim=gamma_val2)

  list2return = list("mae_cal"=mae_cal, "mae_val"=mae_val, "df_cal"=df_cal, "df_val"=df_val)

  if (return_regression == TRUE){
    list2return[["regression"]] = regr
  }
  return(list2return)

}
