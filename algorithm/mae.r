
mae <- function(obs, sim){
  mae <- mean(abs(obs-sim))
  return(mae)
}

limit_gamma_to_be_valid <- function(values,
                        upper_limit,
                        lower_limit){
  values_limited <- ifelse(values > upper_limit, 5,
                            ifelse(values < lower_limit, 0.1, values))
  return(values_limited)
}


logmae <- function(obs, sim){
  obs = log(obs+1)
  sim = log(sim+1)
  mae <- mean(abs(obs-sim))
  return(mae)
}
