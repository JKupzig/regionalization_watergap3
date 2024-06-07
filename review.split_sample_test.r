source("./algorithm/apply.MultipleLinearRegression.R")
source("./algorithm/apply.knn.R")
source("./algorithm/apply.RandomForest.R")
source("./algorithm/apply.Beck2016.R")
source("./algorithm/apply.kmeans.R")
source("./algorithm/apply.spatialProximity.R")

split_sample_test<- function(nrepeats=1000,
                          column_idx,
                          tuning_pars,
                          catchment_distances,
                          catchment_characteristics,
                          catchment_gamma)
  {

  set.seed(123)
  results <- matrix(NA, nrow=nrepeats, ncol=32)
  pb <- txtProgressBar(min = 1, max = nrepeats, style = 3)

  cols_wg2 <- c(19, 10, 17, 18, 5)

  #split sample test
  for (i in 1:nrepeats){

    setTxtProgressBar(pb, i)
    ind <- sample(2, nrow(catchment_characteristics), replace=TRUE, prob=c(0.50, 0.50))


    #Müller Schmied approach to compare
    colWG2 <- c(2, 19, 5, 18, 10, 17)
    maes_MLR_WG2 <- apply.MultipleLinearRegression(ind, catchment_characteristics[colWG2], catchment_gamma, ln="on")
        results[i,1] <- maes_MLR_WG2[[1]] #cal
        results[i,2] <- maes_MLR_WG2[[2]] #val

    #Own MLR approach
    maes_MLR <- apply.MultipleLinearRegression(ind, catchment_characteristics[column_idx], catchment_gamma)
        results[i,3] <- maes_MLR[[1]] #cal
        results[i,4] <- maes_MLR[[2]] #val

    maes_MLR_t <- apply.MultipleLinearRegression(ind, catchment_characteristics[column_idx], catchment_gamma,
                                                 mod=T, tuningPars=tuning_pars)
        results[i,5] <- maes_MLR_t[[1]] #cal
        results[i,6] <- maes_MLR_t[[2]] #val

    #Random Forest as ML approach for regression
    maes_RF <- apply.RandomForest(ind, catchment_characteristics[column_idx], catchment_gamma,
                                  mod=F, tuningPars=tuning_pars,
                                  NTrees=200, mTRY=2)
        results[i,7] <- maes_RF[[1]] #cal
        results[i,8] <- maes_RF[[2]] #val

    maes_RF_t <- apply.RandomForest(ind, catchment_characteristics[column_idx], catchment_gamma,
                                  mod=T, tuningPars=tuning_pars,
                                  NTrees=200, mTRY=2)
        results[i,9] <- maes_RF_t[[1]] #cal
        results[i,10] <- maes_RF_t[[2]] #val

    maes_RF_all <- apply.RandomForest(ind, catchment_characteristics[column_idx], catchment_gamma,
                                        mod=F, tuningPars=tuning_pars,
                                        NTrees=200, mTRY=length(column_idx))
        results[i,29] <- maes_RF_all[[1]] #cal
        results[i,30] <- maes_RF_all[[2]] #val

    #############################################################have to work further!
    #SI after Beck et al. 2016
    maes_Beck1 <- apply.Beck2016(catchment_characteristics[column_idx], catchment_gamma, ind,
                                 n_ensemble=1, mod=F, tuningPars=tuning_pars)
       results[i,11] <- maes_Beck1[[1]] #cal
       results[i,12] <- maes_Beck1[[2]] #val

   maes_Beck10 <- apply.Beck2016(catchment_characteristics[column_idx], catchment_gamma, ind,
                                 n_ensemble=10, tuningPars=tuning_pars)
      results[i,13] <- maes_Beck10[[1]] #cal
      results[i,14] <- maes_Beck10[[2]] #val


   maes_Beck10_t <- apply.Beck2016(catchment_characteristics[column_idx], catchment_gamma, ind,
                                   n_ensemble=10, mod=T, tuningPars=tuning_pars)
     results[i,15] <- maes_Beck10_t[[1]] #cal
     results[i,16] <- maes_Beck10_t[[2]] #val

    maes_Beck1_t <- apply.Beck2016(catchment_characteristics[column_idx], catchment_gamma, ind,
                                     n_ensemble=1, mod=T, tuningPars=tuning_pars)
     results[i,31] <- maes_Beck1_t[[1]] #cal
     results[i,32] <- maes_Beck1_t[[2]] #val

  #KNN as clustering approach
  maes_knn <-apply.kmeans(ind, catchment_characteristics[column_idx], catchment_gamma, normType="MinMax",
                           n_centers = 3, mod=F,tuningPars=tuning_pars )
        results[i,17] <- maes_knn[[1]] #cal
        results[i,18] <- maes_knn[[2]] #val

  maes_knn_t <-apply.kmeans(ind, catchment_characteristics[column_idx], catchment_gamma, normType="MinMax",
                             n_centers = 3, mod=T,tuningPars=tuning_pars )
        results[i,19] <- maes_knn_t[[1]] #cal
        results[i,20] <- maes_knn_t[[2]] #val

  maes_knn_flexible <-apply.knn(ind,
                                catchment_characteristics[column_idx],
                                catchment_gamma)
        results[i,21] <- maes_knn_flexible[[1]] #cal
        results[i,22] <- maes_knn_flexible[[2]] #val


  mae_sp_3 <- apply.spatialProximity(ind, catchment_distances, catchment_gamma, k=1)
  results[i,23] <- mae_sp_3[[1]]
  results[i,24] <- mae_sp_3[[2]]

  mae_sp_4 <- apply.spatialProximity(ind, catchment_distances, catchment_gamma, k=10)
  results[i,25] <- mae_sp_4[[1]]
  results[i,26] <- mae_sp_4[[2]]


  mae_sp_8 <- apply.spatialProximity(ind, catchment_distances, catchment_gamma,
                                     k=10, mod=T,tuningPars=tuning_pars)
  results[i,27] <- mae_sp_8[[1]]
  results[i,28] <- mae_sp_8[[2]]

  }

  df_results <- as.data.frame(results)
  names(df_results) <- c("cal_WG2",     "val_WG2",
                         "cal_MLR",     "val_MLR",
                         "cal_MLR_t",   "val_MLR_t",
                         "cal_RF",      "val_RF",
                         "cal_RF_t",    "val_RF_t",
                         "cal_SI_1",   "val_SI_1",
                         "cal_SI_10",  "val_SI_10",
                         "cal_SI_10_t","val_SI_10_t",
                         "cal_k-means",     "val_k-means",
                         "cal_k-means_t",   "val_k-means_t",
                         "cal_knn",  "val_knn",
                         "cal_SP_1",    "val_SP_1",
                         "cal_SP_10",   "val_SP_10",
                         "cal_SP_10_t",   "val_SP_10_t",
                         "cal_RF_all", "val_RF_all",
                         "cal_SI_1_t",   "val_SI_1_t")

  return(df_results)
}
