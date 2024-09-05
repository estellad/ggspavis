clusterPlot_by_cluster <- function(sce_obj, annotate = "spatial.cluster", title = " ", vec = NULL){
  # vec <- as.factor(sce_obj[[annotate]])
  vec <- as.factor(vec)
  for (j in 1:nlevels(vec)){
    sce_obj[[levels(vec)[j]]] <- ifelse(sce_obj[[annotate]] == levels(vec)[j], TRUE, FALSE)
  }
  clus <- levels(vec)
  feat.plots <- purrr::map(clus, function(x) clusterPlot(sce_obj, label = x, palette = c("grey", "red")) +
                             theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
                             ggtitle(x))
  clusters_plot <- patchwork::wrap_plots(feat.plots, ncol=3) +
    plot_annotation(title = title) &
    theme(plot.title = element_text(hjust = 0.5))
  
  return(clusters_plot)
}

plotSpotQC(sce, annotate = "Tumor_pure", x_coord = "pxl_col_in_fullres", y_coord = "pxl_row_in_fullres", type = "spots", y_reverse = FALSE) + 
  scale_x_reverse() + 
  theme(legend.position = "none") + 
  ggtitle ("Tumor_pure") 