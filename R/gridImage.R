

sliceImage <- function(volume,
                       dimension = 2,
                       slice = NULL,
                       low = min(volume, na.rm = TRUE),
                       high = max(volume, na.rm = TRUE),
                       reverse = FALSE, underTransparent = FALSE,
                       col = gray.colors(255),
                       symmetric=FALSE,
                       rCol = defaultRCol(),
                       alpha=NULL,
                       box=FALSE,
                       vp=NULL) {

  if (length(dim(volume)) != 3)
    stop("volume must be 3 dimensional, you may be missing a call to mincArray")
  s <- RMINC:::getSlice(volume, slice, dimension)
  if (reverse) {
    m <- -1
  }
  else {
    m <- 1
  }

  if (!is.null(alpha)) {
    alphastr <- as.hexmode(floor(alpha*255))
    col = paste0(substr(col, 1, 7), alphastr)
    rCol = paste0(substr(rCol, 1, 7), alphastr)
  }

  imRange <- RMINC:::getRangeFromHistogram(volume, low, high)
  s$slice <- RMINC:::scaleSlice(s$slice, imRange[1] * m, imRange[2] *
                          m, underTransparent = underTransparent)
  sliceDims <- dim(s$slice)
  #if (!all(is.na(s$slice))) {
    colourDepth <- length(col)
    paletteScaledSlice <- RMINC:::scaleSliceToPalette(s$slice, low,
                                              high, col)
    colourizedSlice <- col[paletteScaledSlice]
    dim(colourizedSlice) <- sliceDims
    flip_option <- getOption("RMINC_flip_image", TRUE)
    if (flip_option)
      colourizedSlice <- colourizedSlice[, sliceDims[2]:1]
    colourizedSlice <- t(colourizedSlice)
    #rasterImage(colourizedSlice, xleft = 0, xright = sliceDims[1],
    #            ytop = 0, ybottom = sliceDims[2])
    g <- rasterGrob(colourizedSlice, vp=vp) #
    #grid.raster(colourizedSlice)
  #
  #return(invisible(NULL))
    if (symmetric) {
      return(gList(g, sliceImage(volume, dimension,
                                 slice=slice,
                                 low = low,
                                 high=high,
                                 reverse=TRUE,
                                 underTransparent=TRUE,
                                 col=rCol, vp=vp)))
    } else {
  return(gList(g, rectGrob(gp=gpar(col="black", fill="#FFFFFF00"), vp=vp)))
    }
}

sliceContours <- function(volume,
                          dimension=2,
                          slice=NULL,
                          levels=NULL,
                          vp=NULL,
                          col="white",
                          lty=1,
                          lwd=1) {
  if (is.null(levels)) stop("must specify levels")
  s <- RMINC:::getSlice(volume, slice, dimension)
  d <- dim(s$slice)

  colVector <- col[floor(seq.int(from=1, to=length(col), length.out = length(levels)))]
  ltyVector <- lty[floor(seq.int(from=1, to=length(lty), length.out = length(levels)))]
  lwdVector <- lwd[floor(seq.int(from=1, to=length(lwd), length.out = length(levels)))]


  lines <- contourLines(1:d[1], 1:d[2], s$slice, levels=levels) %>%
    map( ~ linesGrob(.$x, .$y, vp=vp, default.units = "native",
                     gp=gpar(col=colVector[match(.$level, levels)],
                             lty=ltyVector[match(.$level, levels)],
                             lwd=lwdVector[match(.$level, levels)]))) %>%
    do.call(gList, .)
  return(lines)
}



defaultCol <- function() {
  getOption("MRIcrotomeCol", colorRampPalette(c("red", "yellow"))(255))
}

defaultRCol <- function() {
  getOption("MRIcrotomeRcol", colorRampPalette(c("blue", "turquoise1"))(255))
}



theme_black=function(base_size=12,base_family="") {
  theme_grey(base_size=base_size,base_family=base_family) %+replace%
    theme(
      # Specify axis options
      axis.line=element_line(colour="white"), #element_blank(),
      axis.text.x=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,vjust=1),
      axis.text.y=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,hjust=1),
      axis.ticks=element_line(color="white",size = 0.2),
      axis.title.x=element_text(size=base_size,color="white",vjust=1, margin=margin(5,0,0,0)),
      axis.title.y=element_text(size=base_size,color="white",angle=90,
                                vjust=0.5, margin=margin(0,5,0,0)),
      axis.ticks.length=unit(0.3,"lines"),
      #axis.ticks.margin=unit(0.5,"lines"),
      # Specify legend options
      legend.background=element_rect(color=NA,fill="black"),
      legend.key=element_rect(color=NA, fill="black"),
      legend.key.size=unit(1.2,"lines"),
      legend.key.height=NULL,
      legend.key.width=NULL,
      legend.text=element_text(size=base_size*0.8,color="white"),
      legend.title=element_text(size=base_size*0.8,face="bold",hjust=0,
                                color="white"),
      legend.position="right",
      legend.text.align=NULL,
      legend.title.align=NULL,
      legend.direction="vertical",
      legend.box=NULL,
      # Specify panel options
      panel.background=element_rect(fill="black",color = NA),
      panel.border=element_blank(), #element_rect(fill=NA,color="white"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      #panel.margin=unit(0.25,"lines"),
      # Specify facetting options
      strip.background=element_rect(fill="grey30",color="grey10"),
      strip.text.x=element_text(size=base_size*0.8,color="white"),
      strip.text.y=element_text(size=base_size*0.8,color="white",
                                angle=-90),
      # Specify plot options
      plot.background=element_rect(color="black",fill="black"),
      plot.title=element_text(size=base_size*1.2,color="white"),
      plot.margin=unit(c(1,1,0.5,0.5),"lines")
    )
}
