
source("./algorithm/mae.r")

run.MultipleLinearRegression <- function(ind,
                             x,
                             y,
                             tuningPars=c(1.18,3.48),
                             ln="off",
                             return_regression = FALSE){

  data.training <- x[ind==1, ]
  data.test <- x[ind==2, ]

  data.training$gamma <- y[ind==1]
  data.test$gamma <- y[ind==2]

  lower <- tuningPars[1]
  upper <- tuningPars[2]

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

  gamma_cal_limited <- limit_gamma_to_be_valid(gamma_cal, upper_limit=5, lower_limit=0.1)
  gamma_val_limited <- limit_gamma_to_be_valid(gamma_val, upper_limit=5, lower_limit=0.1)

  gamma_cal_tuned <- limit_gamma_to_be_valid(gamma_cal, upper_limit=upper, lower_limit=lower)
  gamma_val_tuned <- limit_gamma_to_be_valid(gamma_val, upper_limit=upper, lower_limit=lower)

  if (return_regression == TRUE){
    return(regr)
  } else {
    return(list(gamma_cal_limited=gamma_cal_limited,
                gamma_val_limited=gamma_val_limited,
                gamma_cal_tuned=gamma_cal_tuned,
                gamma_val_tuned=gamma_val_tuned,
                gamma_cal_calibrated=data.training$gamma,
                gamma_val_calibrated=data.test$gamma))
  }
}


apply.MultipleLinearRegression <- function(ind,
                                           x,
                                           y,
                                           tuningPars=c(1.18,3.48),
                                           ln="off") {

  precictions <- run.MultipleLinearRegression(
    ind=ind,
    x=x,
    y=y,
    tuningPars=tuningPars,
    ln=ln)

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
