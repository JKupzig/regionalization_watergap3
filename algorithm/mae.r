
mae <- function(obs, sim){
  mae <- mean(abs(obs-sim))
}

limit_gamma_to_be_valid <- function(values,
                        upper_limit,
                        lower_limit){
  values_limited <- ifelse(values > upper_limit, 5,
                            ifelse(values < lower_limit, 0.1, values))
  return(values_limited)
}
