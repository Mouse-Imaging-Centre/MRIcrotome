#' Title
#'
#' @param data 
#'
#' @returns
#' @export
#'
#' @examples
MRIcroscope <- function(data) {
  plot <- ggplot2::ggplot() + 
    ggplot2::coord_sf() +
    ggplot2::theme(axis.ticks = element_blank(), panel.grid = element_blank(), 
          panel.background = element_blank(), axis.text = element_blank())
  plot$MRIcrotome <- data
  return(plot)
}

#' Title
#'
#' @param plot 
#' @param data 
#'
#' @returns
#' @export
#'
#' @examples
add_roi_outline <- function(plot, data=NULL) {
  if(is.null(data)) {
    data <- plot$MRIcrotome$data
  }
  plot <- plot + tidyterra::geom_spatvector(data=data, fill=NA)
  return(plot)
}

#' Title
#'
#' @param plot 
#' @param data 
#' @param low 
#' @param high 
#' @param guide 
#'
#' @returns
#' @export
#'
#' @examples
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

#' Title
#'
#' @param plot 
#' @param column 
#' @param variable 
#' @param data 
#' @param low 
#' @param high 
#' @param symmetric 
#'
#' @returns
#' @export
#'
#' @examples
add_roi_overlay <- function(plot, column, variable="var", name = "roi", data=NULL, 
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
    scale_fill_posneg(name, low=low, high=high)
  return(plot)
  
}

#' Title
#'
#' @param plot 
#' @param data 
#' @param name
#' @param low 
#' @param high 
#' @param symmetric 
#'
#' @returns
#' @export
#'
#' @examples
add_voxel_overlay <- function(plot, data, name="voxels", low=2, high=5, symmetric=T) {
  # TODO - add some data type checks and possible conversions
  plot <- plot + 
  tidyterra::geom_spatraster(data=data) +
  scale_fill_posneg(name, low=low, high=high)
  return(plot)
}


#' Title
#'
#' @param ... 
#' @param aesthetic 
#' @param low 
#' @param high 
#'
#' @returns
#' @export
#'
#' @examples
scale_fill_posneg <- function(name = waiver(), ..., aesthetic="fill", low=NULL, high=NULL) {
  if (is.null(low))
    low <- 0
  if (is.null(high))
    high <- max(breaks)
  continuous_scale(aesthetic, name=name, 
                   palette = scales::pal_gradient_n(c("turquoise1", 
                                              "blue", 
                                              "transparent", 
                                              "transparent", 
                                              "red", 
                                              "yellow"),
                                              values = scales::rescale(c(-high, -low, -low + 0.0001,
                                                                       low - 0.0001, low, high))),
                   breaks = c(-high, -low, low, high),
                   limits=c(-high, high),
                   oob=scales::squish, 
                   na.value="transparent",
                   guide="colourbar", ...)
}

testPalette <- function(colours, values, low, high, lowalpha=0) {
  pfun <- scales::pal_gradient_n(c("turquoise1", 
                                   "blue", 
                                   "transparent", 
                                   "red", 
                                   "yellow"),
                                 values = scales::rescale(c(-high, -0.0001, 0, 0.0001, high)))
  function(x) {
    #browser()
    cols <- pfun(x)
    ll <- length(cols)
    
    # if (ll > length(values)) {
    #   
    #   midpoint <- round(ll/2)
    #   uThreshPoint <- round(((ll-midpoint) * (low/high)) + midpoint)
    #   lThreshPoint <- round(midpoint - ((ll-midpoint) * (low/high)))
    #   cols[lThreshPoint:uThreshPoint] <- alpha(cols[lThreshPoint:uThreshPoint], lowalpha)
    #   cat(ll, lThreshPoint, uThreshPoint, "\n")
    #   #cols[midpoint:uThreshPoint] <- alpha(cols[midpoint:uThreshPoint], lowalpha)
    # }
    
    if (ll > length(values)) {
      
      midpoint <- 0.5
      uThreshPoint <- (midpoint*(low/high)) + midpoint
      lThreshPoint <- midpoint - (midpoint*(low/high))
      cols[x >= lThreshPoint & x <= uThreshPoint] <- alpha(cols[x >= lThreshPoint & x <= uThreshPoint], lowalpha)
      cat(ll, lThreshPoint, uThreshPoint, "\n")
      #cols[midpoint:uThreshPoint] <- alpha(cols[midpoint:uThreshPoint], lowalpha)
    }
    return(cols)
  }
}

testPalette2 <- function(colours, values, low, high) {
  pfun <- scales::pal_gradient_n(c("turquoise1", 
                                   "blue", 
                                   "white", 
                                   "red", 
                                   "yellow"),
                                 values = scales::rescale(c(-high, -0.000001, 0, 0.000001, high)))
  #pfun <- scales::pal_gradient_n(c("blue", "white", "red"), values=scales::rescale(c(-high, 0, high)))
  function(x) {
    #browser()
    cols <- pfun(x)
    ll <- length(cols)
    
    #if (ll > length(values)) {
      #browser()
      midpoint <- 0.5
      thresh <- midpoint*(low/high)
      #uThreshPoint <- (midpoint*(low/high)) + midpoint
      #lThreshPoint <- midpoint - (midpoint*(low/high))
      #cols[x >= lThreshPoint & x <= uThreshPoint] <- alpha(cols[x >= lThreshPoint & x <= uThreshPoint], lowalpha)
      #cat(ll, lThreshPoint, uThreshPoint, "\n")
      alphas <- abs((x - midpoint)/thresh)^2
      #alphas <- abs((x - midpoint)/thresh)
      #browser()
      alphas[alphas>1] <- 1
      #cols <- alpha(cols, alphas)
      cols <- colorspace::adjust_transparency(cols, alphas)
      #browser()
      #print(data.frame(x, alphas))
      #cols[midpoint:uThreshPoint] <- alpha(cols[midpoint:uThreshPoint], lowalpha)
    #}
    return(cols)
  }
}

scale_fill_posneg2 <- function(name = waiver(), ..., aesthetic="fill", low=NULL, high=NULL, threshold=1, lowalpha=0.3) {
  if (is.null(low))
    low <- 0
  if (is.null(high))
    high <- max(breaks)
  continuous_scale(aesthetic, name=name, 
                   palette = testPalette2(c("turquoise1", 
                                                      "blue", 
                                                      "white", 
                                                      "red", 
                                                      "yellow"),
                                          values = scales::rescale(c(-high, -0.000001, 0, 0.000001, high)), 
                                          low, 
                                          high),
                   breaks = c(-high, -low, low, high),
                   limits=c(-high, high),
                   oob=scales::squish, 
                   na.value="transparent",
                   guide="colourbar", ...)
}