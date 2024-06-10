create_boxplots <- function(data_1, data_2, data_3, data_4,
                            data_5=NULL, data_6=NULL, data_7=NULL){
  require(ggplot2)

  v1_new <- tidyr::pivot_longer(data_1, cols=names(data_1)[1:ncol(data_1)-1])
  v2_new <- tidyr::pivot_longer(data_2, cols=names(data_2)[1:ncol(data_2)-1])
  v3_new <- tidyr::pivot_longer(data_3, cols=names(data_3)[1:ncol(data_3)-1])
  v4_new <- tidyr::pivot_longer(data_4, cols=names(data_4)[1:ncol(data_4)-1])

  df_new <- rbind(v1_new, v2_new, v3_new, v4_new)

  if (!is.null(data_5)){
    v5_new <- tidyr::pivot_longer(data_5, cols=names(data_5)[1:ncol(data_5)-1])
    df_new <- rbind(df_new, v5_new)
  }

  if (!is.null(data_6)){
    v6_new <- tidyr::pivot_longer(data_6, cols=names(data_6)[1:ncol(data_6)-1])
    df_new <- rbind(df_new, v6_new)
  }

  if (!is.null(data_7)){
    v7_new <- tidyr::pivot_longer(data_7, cols=names(data_7)[1:ncol(data_7)-1])
    df_new <- rbind(df_new, v7_new)
  }


  df_new$tuned <- sapply(1:length(df_new$name), function(x) {
    unlist(gregexpr("_t", df_new$name[x]))
  })
  df_new$tuned_str <- if_else(df_new$tuned < 0, "", "*") #tuned &

  df_new$ens <- sapply(1:length(df_new$name), function(x) {
    unlist(gregexpr("_10", df_new$name[x]))
  })
  df_new$ens_str <- dplyr::if_else(df_new$ens < 0, "", "\n ensemble")


  df_new$input_type <- unlist(lapply(1:nrow(df_new), function(x) {
    strsplit(df_new$name[x], "_")[[1]][1]
  }))
  df_new$method <- unlist(lapply(1:nrow(df_new), function(x) {
    strsplit(df_new$name[x], "_")[[1]][2]
  }))
  df_new$name_to_plot <- sapply(1:length(df_new$name), function(x) {
    sprintf("%s%s %s", df_new$info[x], df_new$tuned_str[x], df_new$ens_str[x])
  })


  boxplot_plot <- ggplot(df_new,
                         aes(x = name_to_plot,
                             y = value,
                             color = input_type),
                         facets=~method) +
    geom_boxplot() +
    xlab("") +
    ylab("logMAE") +
    facet_grid(. ~method, scales = "free_x", space = "free_x") +
    theme_bw() +
    scale_color_manual(values = c("cornflowerblue", "firebrick"),
                       labels = c("train", "test"))


  print(boxplot_plot)
  return(boxplot_plot)
}
