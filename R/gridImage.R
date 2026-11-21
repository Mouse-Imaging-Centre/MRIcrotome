

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
                       interpolate = TRUE,
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

  imRange <- getRange(volume, low, high)
  s$slice <- scaleSlice(s$slice, imRange[1] * m, imRange[2] *
                          m, underTransparent = underTransparent)
  sliceDims <- dim(s$slice)

  if (!is.null(alpha)) {
    if (is.numeric(s$slice)) {
      col <- setAlpha(col, alpha)
      rCol <- setAlpha(rCol, alpha)
    }
    else {
      s$slice[!is.na(s$slice)] <- setAlpha(s$slice[!is.na(s$slice)], alpha)
      dim(s$slice) <- sliceDims
    }
  }

  #if (!all(is.na(s$slice))) {
  if (is.numeric(s$slice)) {
    colourDepth <- length(col)
    paletteScaledSlice <- scaleSliceToPalette(s$slice, imRange[1],
                                              imRange[2], col)
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
                    height = unit(sliceDims[2], "native"), interpolate = interpolate) #
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
                                 col=rCol, vp=vp, interpolate = interpolate)))
    } else {
  #return(gList(g, rectGrob(gp=gpar(col="black", fill="#FFFFFF00"), vp=vp)))
      return(gList(g))
    }
}

# Colours with their opacity set to alpha (0 = transparent, 1 = opaque). Takes
# any colour that R knows: names such as "red", "#RRGGBB" and "#RRGGBBAA".
setAlpha <- function(colours, alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1)
    stop("'alpha' must be one number between 0 and 1")
  m <- col2rgb(colours)
  rgb(m[1, ], m[2, ], m[3, ], alpha = floor(alpha * 255), maxColorValue = 255)
}

# Native y position for a y position measured from the first voxel row.
# sliceImage() puts the first row at the bottom when the RMINC_flip_image
# option is TRUE (the default) and at the top when it is FALSE; contours and
# slice indicator lines go through here so that they follow the image.
flipNativeY <- function(y, height) {
  if (getOption("RMINC_flip_image", TRUE)) y else height - y
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
  # an empty cell of the grid (no slice) or a flat slice has no contours (R
  # 4.7 warns "all z values are equal" for a flat slice, e.g. one without the
  # label of contours(labels = ))
  if (length(unique(s$slice[!is.na(s$slice)])) < 2) return(gList())

  colVector <- col[floor(seq.int(from=1, to=length(col), length.out = length(levels)))]
  ltyVector <- lty[floor(seq.int(from=1, to=length(lty), length.out = length(levels)))]
  lwdVector <- lwd[floor(seq.int(from=1, to=length(lwd), length.out = length(levels)))]


  # voxel i fills native coordinates i-1 to i, so its centre is at i - 0.5
  lines <- contourLines(seq_len(d[1]) - 0.5, seq_len(d[2]) - 0.5, s$slice, levels=levels) %>%
    map( ~ linesGrob(.$x, flipNativeY(.$y, d[2]), vp=vp, default.units = "native",
                     gp=gpar(col=colVector[match(.$level, levels)],
                             lty=ltyVector[match(.$level, levels)],
                             lwd=lwdVector[match(.$level, levels)])))
  return(do.call(gList, lines))
}



#' Default colour palette for overlays
#'
#' Returns the default colour palette used by \code{\link{overlay}}: a
#' red-to-yellow ramp with 255 colours. Set the \code{MRIcrotomeCol} option
#' to change it, e.g. \code{options(MRIcrotomeCol = heat.colors(255))}.
#'
#' @return A character vector of colours.
#' @export
#' @examples
#' defaultCol()
#' options(MRIcrotomeCol = colorRampPalette(c("white", "red"))(255))
#' defaultCol()[1]
#' options(MRIcrotomeCol = NULL)
defaultCol <- function() {
  getOption("MRIcrotomeCol", colorRampPalette(c("red", "yellow"))(255))
}

#' Default reverse colour palette for overlays
#'
#' Returns the default colour palette used for negative values when
#' \code{symmetric=TRUE} in \code{\link{overlay}}: a blue-to-turquoise ramp
#' with 255 colours. Set the \code{MRIcrotomeRcol} option to change it, e.g.
#' \code{options(MRIcrotomeRcol = cm.colors(255))}.
#'
#' @return A character vector of colours.
#' @export
#' @examples
#' defaultRCol()
defaultRCol <- function() {
  getOption("MRIcrotomeRcol", colorRampPalette(c("blue", "turquoise1"))(255))
}

# Colour range for a volume; whichever of low/high is NULL gets the robust
# 2nd/98th percentile (the same "robust range" FSL uses), so a few extreme
# voxels do not dictate the scale.
getRange <- function (volume, low = NULL, high = NULL) {
  if (is.character(volume)) return(c(NA, NA))
  if (is.null(low) || is.null(high)) {
    if (all(is.na(volume)))
      stop("Cannot choose a colour range: the volume has only NA values. ",
           "Give 'low' and 'high'.")
    q <- quantile(volume, c(0.02, 0.98), na.rm = TRUE, names = FALSE)
    # sparse volumes (e.g. a few labels on a zero background) collapse the
    # robust range to a single value; use the full range instead
    if (q[1] == q[2]) q <- range(volume, na.rm = TRUE)
    if (is.null(low)) low <- q[1]
    if (is.null(high)) high <- q[2]
  }
  c(low, high)
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

  slice[slice < 0] <- under
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
