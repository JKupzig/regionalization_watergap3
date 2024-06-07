source("./algorithm/mae.r")

run.kmeans <- function(
    ind,
    x,
    y,
    normType="MinMax",
    n_centers = 3,
    tuningPars=c(1.16, 3.5)){

  # ============================================================================
  # Normalization of Variables
  # ============================================================================

  # often it is preferred to use normalized values
  # Stabdardization (min/max)

  standardize <- function(x) {
    num <- x - mean(x)
    denom <-  sd(x)
    return (num/denom)
  }

  normalizeMinMax <- function(x) {
    num <- x - min(x)
    denom <- max(x) - min(x)
    return (num/denom)
  }

  normalizeL1 <- function(x) {
    num <- x
    denom <- sum(abs(x))
    return (num/denom)
  }

  normalizeL2 <- function(x) {
    num <- x
    denom <- sqrt(sum(x^2))
    return (num/denom)
  }

  data.training <- x[ind==1, ]
  data.test     <- x[ind==2, ]

  lower = tuningPars[1]
  upper = tuningPars[2]

  data.training.long <- gather(data.training, factor_key=TRUE)
  info <- data.training.long%>% group_by(key)%>%
    summarise(mean= mean(value), sd= sd(value), max = max(value),min = min(value))

  gamma.training <- y[ind==1]
  gamma.test     <- y[ind==2]

  #normalization
  if (normType=="MinMax")
  {
    for (column in 1:ncol(data.training)){
      data.test[,column] <-  (data.test[,column]-min(data.training[,column])) / (max(data.training[,column]-min(data.training[,column])))
    }
    data.training <- apply(data.training, 2, normalizeMinMax)

  } else if (normType=="L1")
  { #not really appropiat for my purpose..
    stop("L1-norm not implemented yet")
  } else if (normType=="L2")
  { #not really appropiat for my purpose..
    stop("L2-norm not implemented yet")
  } else {
    for (column in 1:ncol(data.training)){
      data.test[,column] <- (data.test[,column]- as.numeric(info[column,2])) / as.numeric(info[column,3])
    }
    data.training <- apply(data.training, 2, standardize)
  }

  #makes script very slow.. often its three
  #nClusters <- NbClust::NbClust(data=data.training, min.nc=2,max.nc=150, method="kmeans")
  #nCenters <-  #max(nClusters$Best.partition)
  cluster <- stats::kmeans(data.training, centers=n_centers, iter.max = 10, nstart = 50) #gamma besitzt 3Nkst

  data2plot <- data.frame(gamma = gamma.training,
                          variable=as.factor(as.vector(cluster$cluster)))
  gammaGroups <- aggregate(. ~ data2plot$variable, data2plot, function(x) c(mean = mean(x)))
  gammaGroups <- gammaGroups$gamma

  groups_val <- class::knn(train = data.training,
                           test = data.test,
                           cl = as.vector(cluster$cluster),
                           k= 1) #formerly: n_centers (Bug!)

  gamma_cal <- gammaGroups[as.vector(cluster$cluster)]
  gamma_val <- gammaGroups[as.integer(groups_val)]

  gamma_cal_limited <- limit_gamma_to_be_valid(gamma_cal, upper_limit=5, lower_limit=0.1)
  gamma_val_limited <- limit_gamma_to_be_valid(gamma_val, upper_limit=5, lower_limit=0.1)

  gamma_cal_tuned <- limit_gamma_to_be_valid(gamma_cal, upper_limit=upper, lower_limit=lower)
  gamma_val_tuned <- limit_gamma_to_be_valid(gamma_val, upper_limit=upper, lower_limit=lower)

  return(list(gamma_cal_limited=gamma_cal_limited,
              gamma_val_limited=gamma_val_limited,
              gamma_cal_tuned=gamma_cal_tuned,
              gamma_val_tuned=gamma_val_tuned,
              gamma_cal_calibrated=gamma.training,
              gamma_val_calibrated=gamma.test))
}

apply.kmeans <- function(
    ind,
    x,
    y,
    normType="MinMax",
    n_centers = 3,
    tuningPars=c(1.16, 3.5)){

  prediction <- run.kmeans(ind, x, y, normType, n_centers, tuningPars)

  mae_cal <- mae(obs=prediction$gamma_cal_calibrated,
                 sim=prediction$gamma_cal_limited)
  mae_val <- mae(obs=prediction$gamma_val_calibrated,
                 sim=prediction$gamma_val_limited)

  mae_cal_t <- mae(obs=prediction$gamma_cal_calibrated,
                   sim=prediction$gamma_cal_tuned)
  mae_val_t <- mae(obs=prediction$gamma_val_calibrated,
                   sim=prediction$gamma_val_tuned)

  list2return = list("mae_cal"=mae_cal,
                     "mae_val"=mae_val,
                     "mae_cal_t"=mae_cal_t,
                     "mae_val_t"=mae_val_t)
  return(list2return)
}

