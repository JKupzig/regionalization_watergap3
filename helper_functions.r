
read_kge <- function(cont, type, red_y, red_x_orig, ind, ind_type=2){
  kge_cal_cont <- read.table(sprintf("./data/KGE/KGE_%s_%s_extended.txt", type, cont), sep="\t", header=T)
  kge_cal_cont$cont <- cont
  kge_cal_cont$runtype <- type

  missing_basins <- red_y[!(red_y$ID %in% kge_cal_cont$station_id) &
                            red_x_orig$continent == cont, 1]
  if (length(missing_basins) > 0){
    stop(sprintf("something wrong with basins in %s!", cont))
  }

  reduced_kge_cont <- kge_cal_cont[kge_cal_cont$station_id %in% red_y$ID[ind==ind_type],]
  return(reduced_kge_cont)
}


create_subset <- function(min_quality=NULL, min_size = NULL, use_kge="off")
{
  kge <- read.table(file.path("./data", "monthly_kge_below_04.txt"), header=T)
  quality <- readRDS(file.path("./data", "NEW_quality_monthly_bias.rds"))
  id_order <- readRDS(file.path("./data", "NEW_IDs.rds"))
  sorted_quality <- quality[order(match(quality$V1, id_order)), ]

  ind <- rep(TRUE, nrow(quality))

  if (!is.null(min_quality))
  {
    ind_quality <- (sorted_quality[, 2] > (1 - min_quality)) &
      (sorted_quality[, 2] < (1 + min_quality))
    ind <- ind & ind_quality
  }

  if (use_kge == "on")
  {
    ids_to_delete <- which(sorted_quality$V1 %in% kge$ids)
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
