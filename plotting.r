
plot_shadow <- function(results, ylabel, xlabel, name, legend=NULL) {
  if (is.null(legend)){
    legend = "topleft"
  }
  LWD = 2
  CEX = 1.5

  y_min <- min(results) - abs(min(results))*0.11
  y_max <- max(results) + abs(max(results))*0.11
  step_size <- case_when(
    y_max <= 10 ~ 1.,
    y_max > 10 & y_max <= 20 ~ 2,
    y_max > 20 & y_max <= 100 ~ 10,
    y_max > 100 & y_max <= 200 ~ 20,
    y_max > 200 & y_max <= 500 ~ 50,
    y_max > 500 & y_max <= 1000 ~ 100,
    y_max > 1000 & y_max <= 5000 ~ 200,
    y_max < 5000 ~ 1000
  )

  upper_limit <- apply(results, 2, max)
  lower_limit <- apply(results, 2, min)
  mean_line <- apply(results, 2, mean)

  par(mar=c(5,6,4,1)+.1,
      bg=NA)


  plot(1:12, mean_line, type = 'l',
       xlab = xlabel, ylab = ylabel,
       ylim=c(y_min, y_max),
       lwd = LWD, cex=CEX, cex.lab=CEX, cex.axis=CEX,
       col="black", axes=F)
  axis(2, seq(0, y_max, step_size))
  axis(1, seq(1,12,1))
  polygon(c(1:12, rev(1:12)),
          c(upper_limit, rev(lower_limit)),
          col = rgb(169/255, 169/255, 169/255, 0.5))

  dev.copy(png,
           name,
           width=16, height=12, units="cm", res=300)
  dev.off()

}


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

  df_new$method[df_new$method == "WG2"] <- "B2B"
  df_new$name_to_plot[df_new$method == "B2B"] <- paste0("benchm.",
                                                        df_new$tuned_str[df_new$method == "B2B"])

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
