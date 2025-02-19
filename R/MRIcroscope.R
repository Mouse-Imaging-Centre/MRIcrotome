MRIcroscope <- function(data) {
  plot <- ggplot2::ggplot() + 
    ggplot2::coord_sf() +
    ggplot2::theme(axis.ticks = element_blank(), panel.grid = element_blank(), 
          panel.background = element_blank(), axis.text = element_blank())
  plot$MRIcrotome <- data
  return(plot)
}

add_roi_outline <- function(plot, data=NULL) {
  if(is.null(data)) {
    data <- plot$MRIcrotome$data
  }
  plot <- plot + tidyterra::geom_spatvector(data=data, fill=NA)
  return(plot)
}

add_anatomy <- function(plot, data=NULL, low=700, high=1400, guide="none") {
  if(is.null(data)) {
    data <- plot$MRIcrotome$anatomy
  }
  plot <- plot + 
    tidyterra::geom_spatraster(data=data) + 
    ggplot2::scale_fill_gradient("anatomy", 
                                 low="black", 
                                 high="white", 
                                 limits=c(low, high), 
                                 na.value = "transparent", guide = guide) + 
    ggnewscale::new_scale_fill()
  return(plot)

}

add_roi_overlay <- function(plot, column, variable="var", data=NULL, 
                            low=2, high=5, symmetric=T) {
  if(is.null(data)) {
    data <- plot$MRIcrotome$data
  } else {
    if("Node" %in% class(data)) {
      data <- data.tree::ToDataFrameTable(data, "name", deparse(substitute(column)))
    }
    data <- inner_join(plot$MRIcrotome$data, data, by=c("region" = "name"))
  }
  plot <- plot + 
    tidyterra::geom_spatvector(data=data, aes(fill={{column}})) + 
    ggplot2::scale_fill_continuous(variable)
  return(plot)
  
}