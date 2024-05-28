
create_subset <- function(min_quality, min_size = NULL, use_kge="off")
{
  kge <- read.table(file.path("./data", "monthly_kge_below_04.txt"), header=T)
  quality <- readRDS(file.path("./data", "NEW_quality_monthly_bias.rds"))
  id_order <- readRDS(file.path("./data", "NEW_IDs.rds"))
  quality <- quality[order(match(quality$V1, id_order)), ]

  ind <- (quality[, 2] > (1 - min_quality)) &
    (quality[, 2] < (1 + min_quality))

  if (use_kge == "on")
  {
    ids_to_delete <- which(id_order %in% kge$ids)
    ind[ids_to_delete] <- FALSE
  }
  if (!is.null(min_size))
  {
    data <- readRDS(file.path("./data", "NEW_x_orig.rds"))
    ind_size <- data$areaBasin >= min_size
    ind <- ind & ind_size
  }
  return(ind)
}

columns2use <- c("mean_smax", "mean_op_water", "mean_wetland", "areaBasin", "mean_slope",
                 "mean_altitude", "mean_sealedArea", "mean_Forest", "mean_permaglac",
                 "sum_sw","sum_prec", "mean_temp")

get_data_frame <- function(matrix, name_vector, correlation_name=NULL){
  dataframe <- data.frame(correlation=matrix[names(matrix) %in% name_vector],
                          name=names(matrix)[names(matrix) %in% name_vector])
  if (! is.null(correlation_name)){
    names(dataframe)[names(dataframe) == "correlation"] <- correlation_name
  }
  return(dataframe)
}


#K-means for definition of classes
get_classes_for_gamma <- function(gamma, n_centers = 3, print_plot = FALSE){

  gamma_pred <- stats::kmeans(gamma, centers=n_centers, iter.max = 10, nstart = 50)

  #reordering so 1 is always small and 3 is always highest class
  ord <- order(gamma_pred$centers)
  gamma_pred$newClusters <- gamma_pred$cluster

  for (i in 1:n_centers){
    index <- which(gamma_pred$cluster == ord[i])
    gamma_pred$newClusters[index] <- -i
  }

  gamma_pred$newClusters <- gamma_pred$newClusters * -1
  df <- data.frame(gamma=gamma, class = gamma_pred$newClusters)

  if (print_plot == TRUE) {
    plot(df$gamma, df$class)
  }

  predict.kmeans <- function(object, newdata) {

    centers <- object$centers
    ss_by_center <- apply(centers, 1, function(x) {
      colSums((t(newdata) - x) ^ 2)
    })
    best_clusters <- apply(ss_by_center, 1, which.min)
    return(data.frame(data=newdata, predict=best_clusters))
  }

  newClasses <- predict.kmeans(gamma_pred,
    seq(min(gamma), max(gamma), (max(gamma) - min(gamma)) / 500))

  for (i in 1:n_centers){
    index <- which(newClasses$predict == ord[i])
    newClasses$predict[index] <- -i
  }

  newClasses$predict <- newClasses$predict * -1

  if (print_plot == TRUE){
    plot(gamma, gamma_pred$newClusters)
    points(newClasses$data, newClasses$predict, col = "red")
  }

  threshold_info <- matrix(NA, nrow = 2, ncol = n_centers)
  for (i in 1:n_centers){
    threshold_info[1, i] <- min(newClasses$data[newClasses$predict == i])
    threshold_info[2, i] <- max(newClasses$data[newClasses$predict == i])
  }


  rownames(threshold_info) <- c("min", "max")

  return(list(df = df, dfThresholds = threshold_info))

}


new_model <- function(data, coeffs, lower, upper) {
  y_val <- coeffs[1] +
    coeffs[2] * data$mean_slope +
    coeffs[3] * data$mean_Forest +
    coeffs[4] * data$mean_permaglac +
    coeffs[5] * data$mean_temp +
    coeffs[6] * data$sum_sw

  y_val <- ifelse(y_val > upper, 5, y_val)
  y_val <- ifelse(y_val < lower, 0.1, y_val)
  return(y_val)
}



wg2_model <- function(data, coeffs) {
  y_val <- coeffs[1] +
    coeffs[2] * data$mean_smax +
    coeffs[3] * data$mean_temp +
    coeffs[4] * data$mean_op_water +
    coeffs[5] * data$mean_Rg_max +
    coeffs[6] * data$mean_slope +
    coeffs[7] * data$mean_op_water

  y_val = exp(y_val)
  y_val <- ifelse(y_val > 5, 5, y_val)
  y_val <- ifelse(y_val < 0.1, 0.1, y_val)
  return(y_val)
}
