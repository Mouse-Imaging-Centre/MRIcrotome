

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
  s <- getSlice(volume, slice, dimension)
  if (reverse) {
    m <- -1
  }
  else {
    m <- 1
  }

  imRange <- getRangeFromHistogram(volume, low, high)
  s$slice <- scaleSlice(s$slice, imRange[1] * m, imRange[2] *
                          m, underTransparent = underTransparent)
  sliceDims <- dim(s$slice)

  if (!is.null(alpha)) {
    alphastr <- as.hexmode(floor(alpha*255))
    if (is.numeric(s$slice)) {
      col = paste0(substr(col, 1, 7), alphastr)
      rCol = paste0(substr(rCol, 1, 7), alphastr)
    }
    else {
      s$slice[!is.na(s$slice)] <- paste0(substr(s$slice[!is.na(s$slice)], 1, 7), alphastr)
      dim(s$slice) <- sliceDims
    }
  }

  #if (!all(is.na(s$slice))) {
  if (is.numeric(s$slice)) {
    colourDepth <- length(col)
    paletteScaledSlice <- scaleSliceToPalette(s$slice, low,
                                              high, col)
    colourizedSlice <- col[paletteScaledSlice]
    dim(colourizedSlice) <- sliceDims
  }
  else {
    colourizedSlice <- s$slice
  }
    flip_option <- getOption("RMINC_flip_image", TRUE)
    if (flip_option)
      colourizedSlice <- colourizedSlice[, sliceDims[2]:1]
    colourizedSlice <- t(colourizedSlice)
    #rasterImage(colourizedSlice, xleft = 0, xright = sliceDims[1],
    #            ytop = 0, ybottom = sliceDims[2])
    g <- rasterGrob(colourizedSlice, vp=vp, width = unit(sliceDims[1], "native"),
                    height = unit(sliceDims[2], "native")) #
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
  #return(gList(g, rectGrob(gp=gpar(col="black", fill="#FFFFFF00"), vp=vp)))
      return(gList(g))
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
  s <- getSlice(volume, slice, dimension)
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

getRangeFromHistogram <- function (volume, low = NULL, high = NULL) {
  if (is.character(volume)) return(c(NA, NA))
  if(is.null(low) || is.null(high)) hist_midpoints <- hist(volume, plot=F)$mids
  if (is.null(low)) { low <- hist_midpoints[5]}
  if (is.null(high)) { high <- rev(hist_midpoints)[5]}

  return(c(low, high))
}

scaleSlice <- function(slice, low=NULL, high=NULL, underTransparent=TRUE) {
  if (is.character(slice))
    return(slice)

  if (is.null(low)) {
    low <- quantile(slice, 0.5)
  }
  if (is.null(high)) {
    high <- quantile(slice, 0.7)
  }

  # invert if it's a negative scale
  if (high < low) {
    slice <- slice*-1
    high <- high*-1
    low <- low*-1
  }

  slice[slice >= high] <- high
  slice <- slice - low

  if (underTransparent) {
    under <- NA
  }
  else {
    under <- 0
  }

  slice[slice <= 0] <- under
  return(slice)
}

scaleSliceToPalette <- function(slice, low, high, palette){
  if (is.character(slice))
    return(slice)

  dims <- dim(slice)
  slice <- #Scale to 0-1
    slice / abs(high - low)

  maxima <- which(slice == 1)
  slice[maxima] <- slice[maxima] - .Machine$double.eps
  slice <- slice * length(palette)
  slice <- floor(slice) + 1
  dim(slice) <- dims

  return(slice)
}

getSlice <- function(volume, slice, dimension) {
  d <- dim(volume)
  if(length(d) != 3) stop("volume must be 3 dimensional")

  if (dimension == 1) {
    outs <- volume[slice,,]
    outa <- d[3]/d[2]
  }
  else if (dimension == 2) {
    outs <- volume[,slice,]
    outa <- d[3]/d[1]
  }
  else if (dimension == 3) {
    outs <- volume[,,slice]
    outa <- d[2]/d[1]
  }
  else if (dimension > 3) {
    stop("Can only handle three dimensions at the moment")
  }
  return(list(slice=outs, asp=outa))
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
