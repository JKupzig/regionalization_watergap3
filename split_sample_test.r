source("./algorithm/apply.MultipleLinearRegression.R")
source("./algorithm/apply.knn.R")
source("./algorithm/apply.RandomForest.R")
source("./algorithm/apply.Beck2016.R")
source("./algorithm/apply.kmeans.R")
source("./algorithm/apply.spatialProximity.R")

result_array_to_df <- function(result_array){

  result_list <- list()
  for (method in dimnames(result_array)$method){

    column <- which(method == dimnames(result_array)$method)
    result_list[[sprintf("cal_%s", method)]] <- result_array[,column,1]
    result_list[[sprintf("val_%s", method)]] <- result_array[,column,2]
    result_list[[sprintf("cal_%s_t", method)]] <- result_array[,column,3]
    result_list[[sprintf("val_%s_t", method)]] <- result_array[,column,4]
  }
  df_results <- as.data.frame(result_list)

  return(df_results)
}

result_array_to_evaluate_tuning_df <- function(result_array){

  methods <- c()
  types <- c()
  values <- c()
  n_ensemble <- c()

  for (method in dimnames(result_array)$method){

    column <- which(method == dimnames(result_array)$method)

    methods <- c(methods, rep(method, length(result_array[,column,2])*2))
    n_ensemble <- c(seq(1, length(result_array[,column,2]), 1),
                    seq(1, length(result_array[,column,2]), 1))
    types <- c(types,
               rep("orig.", length(result_array[,column,2])),
               rep("tuned", length(result_array[,column,2])))
    values <- c(values,
                result_array[,column,2],
                result_array[,column,4])
  }
  df_results <- data.frame(method=methods, value=values, type=types, n_ensemble=n_ensemble)

  return(df_results)
}

split_sample_test<- function(nrepeats=1000,
                          column_idx,
                          tuning_pars,
                          catchment_distances,
                          catchment_characteristics,
                          catchment_gamma)
  {

  results <- array(NA, dim=c(nrepeats, ncol=8, 4))

  pb <- txtProgressBar(min = 1, max = nrepeats, style = 3)

  cols_wg2 <- c(19, 10, 17, 18, 5)

  # create indexes for ensemble
  set.seed(123)
  index_list <- list()
  for (i in 1:nrepeats)
  {
    ind <- sample(2, nrow(catchment_characteristics), replace=TRUE, prob=c(0.50, 0.50))
    index_list[[i]] <- ind
  }

  #split sample test
  for (i in 1:nrepeats){

    setTxtProgressBar(pb, i)
    ind <- index_list[[i]]

    #Müller Schmied approach to compare
    colWG2 <- c(2, 19, 5, 18, 10, 17)
    maes_MLR_WG2 <- apply.MultipleLinearRegression(
      ind=ind,
      x=catchment_characteristics[colWG2],
      y=catchment_gamma,
      tuningPars=tuning_pars,
      ln="on")
    results[i,1,] <- unlist(maes_MLR_WG2)


    #Own MLR approach
    maes_MLR <- apply.MultipleLinearRegression(
      ind=ind,
      x=catchment_characteristics[column_idx],
      y=catchment_gamma,
      tuningPars=tuning_pars)
    results[i,2,] <- unlist(maes_MLR)

    #Random Forest as ML approach for regression
    maes_RF <- apply.RandomForest(
      ind=ind,
      x=catchment_characteristics[column_idx],
      y=catchment_gamma,
      tuningPars=tuning_pars,
      NTrees=200,
      mTRY=2)
    results[i,3,] <- unlist(maes_RF)

    # maes_RF_flexible <- apply.RandomForest(
    #   ind,
    #   catchment_characteristics[column_idx],
    #   catchment_gamma,
    #   tuningPars=tuning_pars,
    #   NTrees=200,
    #   mTRY=length(column_idx))
    # results[i,4,] <- unlist(maes_RF_flexible)

    #############################################################
    #SI after Beck et al. 2016
    maes_Beck1 <- apply.Beck2016(
      ind=ind,
      x=catchment_characteristics[column_idx],
      y=catchment_gamma,
      n_ensemble=1,
      tuningPars=tuning_pars)
    results[i,4,] <- unlist(maes_Beck1)


   maes_Beck10 <- apply.Beck2016(
     ind=ind,
     x=catchment_characteristics[column_idx],
     y=catchment_gamma,
     n_ensemble=10,
     tuningPars=tuning_pars)
   results[i,5,] <- unlist(maes_Beck10)


    # #KNN as clustering approach
    maes_knn <-apply.kmeans(
      ind=ind,
      x=catchment_characteristics[column_idx],
      y=catchment_gamma,
      normType="MinMax",
      n_centers = 3,
      tuningPars=tuning_pars)
    results[i,6,] <- unlist(maes_knn)

    maes_knn_flexible <-apply.knn(
      ind=ind,
      x=catchment_characteristics[column_idx],
      y=catchment_gamma)
    results[i,7,] <- unlist(maes_knn_flexible)


    mae_sp <- apply.spatialProximity(
      ind=ind,
      x=catchment_distances,
      y=catchment_gamma,
      k=1)
    results[i,8,] <- unlist(mae_sp)

    # mae_sp_n10 <- apply.spatialProximity(
    #   ind,
    #   catchment_distances,
    #   catchment_gamma, k=10)
    # results[i,10,] <- unlist(mae_sp_n10)

  }

  names <- c("WG2", "MLR",
             "RF",
             "SI_1", "SI_10",
             "kmeans", "knn",
             "SP_1")

  dimnames(results) <- list("n_ensemble" = seq(1,nrepeats,1),
                            "method"= names,
                            "mae"=c("cal_mae", "val_mae", "cal_mae_t", "val_mae_t"))

  return(results)
}
