source("./algorithm/mae.r")

apply.kmeans <- function( ind, x, y, normType="MinMax", n_centers = 3, mod=F, tuningPars=c(1.16, 3.5)){


  calcMAE <- function(cluster2use, gammaCal, gammaCenters){

    gamma_cal <- gammaCenters[cluster2use]
    if (mod==T) {
      gamma_cal <- ifelse(as.numeric(gamma_cal)>=tuningPars[2], 5,
                           ifelse(as.numeric(gamma_cal)<=tuningPars[1], 0.1, as.numeric(gamma_cal)))
    }
    mae_cal <- mae(obs=gammaCal, sim=gamma_cal)
  return(mae_cal)
  }



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

  data.training.long <- gather(data.training, factor_key=TRUE)
  info <- data.training.long%>% group_by(key)%>%
    summarise(mean= mean(value), sd= sd(value), max = max(value),min = min(value))


  trainingGamma <- y[ind==1]
  testGamma     <- y[ind==2]

  #getting best n for clustering
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
  #nClusters <- NbClust::NbClust(data=data.training, min.nc=2,max.nc=20, method="kmeans")
  #nCenters <-  #max(nClusters$Best.partition)
  cluster <- stats::kmeans(data.training, centers=n_centers, iter.max = 10, nstart = 50) #gamma besitzt 3Nkst

  data2plot <- data.frame(gamma = trainingGamma, variable=as.factor(as.vector(cluster$cluster)))
  gammaGroups <- aggregate(. ~ data2plot$variable, data2plot, function(x) c(mean = mean(x)))
  gammaGroups <- gammaGroups$gamma

  MAE_cal <- calcMAE(as.vector(cluster$cluster), trainingGamma, gammaGroups)

  gamma_val <- class::knn(train = data.training, test = data.test, cl = as.vector(cluster$cluster), k=n_centers)

  MAE_val <- calcMAE(as.integer(gamma_val), testGamma, gammaGroups)

  list2return = list("mae_cal"=MAE_cal, "mae_val"=MAE_val)

  return(list2return)
}

