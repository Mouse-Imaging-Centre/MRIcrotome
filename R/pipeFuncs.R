initMetaSliceSeries <- function() {
  ssm <- new.env(parent = globalenv())
  ssm$seriesCounter=1
  ssm$ssl=list()
  return(ssm)
}

# Every function that takes a volume goes through here, so a file path or a
# volume that was not passed through mincArray() fails with a clear message
# instead of a cryptic grid error.
validateVolume <- function(volume) {
  if (is.character(volume) && length(volume) == 1)
    stop("'volume' must be a 3D array, not a file path. ",
         "Use mincArray(mincGetVolume(\"", volume, "\")) to load it first.")
  if (inherits(volume, "mincSingleDim"))
    stop("'volume' must be a 3D array. Wrap the output of mincGetVolume() in mincArray().")
  if (!is.array(volume) || length(dim(volume)) != 3)
    stop("'volume' must be a 3D array. Got an object of class ",
         paste(class(volume), collapse = "/"), " with ", length(dim(volume)), " dimensions.")
  invisible(volume)
}


#' Initialize a slice series
#'
#' Starts a new slices series, sets dimensions, slices to use, and rows and
#' columns.
#'
#' @param ssm Provided as part of an ongoing pipe, not usually specified by user
#' @param slices A vector of integers representing which slices to use. Can be
#'   NULL, in which case nrow and ncol together with begin and end are used.
#'   If you give \code{slices} and only one of \code{nrow} or \code{ncol},
#'   the other is calculated from the number of slices. If you give neither,
#'   the slices go in one row (or in the rows of the previous series).
#' @param nrow Rows in the slice series. Set to 1 if NULL and ncol specified.
#' @param ncol Columns in the slice series. Set to 1 if NULL and nrow specified.
#' @param dimension Dimension as integer (1, 2, or 3) for slice series.
#' @param begin First slice. Can be NULL if slices explicitly specified.
#' @param end Last slice. Can be NULL if slices explicitly specified.
#' @param rotate Rotate each slice counterclockwise by 0 (the default), 90,
#'   180 or 270 degrees. For 90 and 270 the slices change from wide to tall,
#'   and the layout of the series follows.
#' @param flip Mirror each slice after the rotation: \code{"none"} (the
#'   default), \code{"horizontal"} (left and right change places) or
#'   \code{"vertical"} (top and bottom change places).
#'
#'   Like the other arguments, \code{rotate} and \code{flip} carry over to
#'   later series in the pipe unless you give them again. They apply to each
#'   layer of the series (anatomy, overlays and contours), so the layers stay
#'   aligned. Slice indicators are not turned: they show a view along another
#'   dimension.
#'
#' @return Returns the slices series info for passing along the pipe.
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics") %>%
#'   draw()
#'
#' # explicit slices; ncol is calculated from their number
#' sliceSeries(nrow = 1, slices = c(15, 25, 35)) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   draw()
#'
#' # the same slices turned a quarter turn, then mirrored
#' sliceSeries(nrow = 1, slices = c(15, 25, 35), rotate = 90) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   sliceSeries(flip = "horizontal") %>%
#'   anatomy() %>%
#'   draw()
sliceSeries <- function(ssm=NULL,
                        slices=NULL,
                        nrow=NULL,
                        ncol=NULL,
                        dimension=NULL,
                        begin=NULL,
                        end=NULL,
                        rotate=NULL,
                        flip=NULL) {

  checkCount <- function(x, arg) {
    if (!is.null(x) && (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1 || x != round(x)))
      stop("'", arg, "' must be one whole number of 1 or more")
  }
  checkCount(nrow, "nrow")
  checkCount(ncol, "ncol")
  if (!is.null(dimension) && (!is.numeric(dimension) || length(dimension) != 1 ||
                              is.na(dimension) || !(dimension %in% 1:3)))
    stop("'dimension' must be 1, 2, or 3")
  if (!is.null(rotate) && (!is.numeric(rotate) || length(rotate) != 1 ||
                           !(rotate %in% c(0, 90, 180, 270))))
    stop("'rotate' must be 0, 90, 180 or 270")
  if (!is.null(flip)) flip <- match.arg(flip, c("none", "horizontal", "vertical"))

  # with explicit slices, calculate the missing nrow or ncol from their number
  if (!is.null(slices)) {
    n <- length(slices)
    if (is.null(nrow) && is.null(ncol))
      nrow <- if (is.null(ssm)) 1 else ssm$ssl[[ssm$seriesCounter]]$nrow
    if (is.null(ncol)) ncol <- ceiling(n / nrow)
    if (is.null(nrow)) nrow <- ceiling(n / ncol)
  }

  # ssm (the first arg) is provided if this is part of an ongoing pipe, but should be
  # null if this is the first initialization
  if (is.null(ssm)) {
    ssm <- initMetaSliceSeries()
    # it is the first time sliceSeries was called in a pipe,
    # so check that necessary args are present
    if (is.null(nrow) && is.null(ncol)) stop("Must specify either nrow or ncol or both")

    # if only one of nrow or ncol is specified, set the other to 1
    if (is.null(nrow)) nrow <- 1
    if (is.null(ncol)) ncol <- 1

    # use a default dimension of 2
    if (is.null(dimension)) dimension <- 2

  }
  else {
    ssm <- cloneSSM(ssm)
    ssm$seriesCounter = ssm$seriesCounter+1
  }
  # add checking for slices vs nrow and ncol

  if (is.null(nrow)) nrow <- ssm$ssl[[ssm$seriesCounter-1]]$nrow
  if (is.null(ncol)) ncol <- ssm$ssl[[ssm$seriesCounter-1]]$ncol

  # initialize sliceSeries list; in all cases use the previous sliceSeries' values
  # if they were not specified here
  prev <- if (ssm$seriesCounter > 1) ssm$ssl[[ssm$seriesCounter-1]] else list()
  l <- list(nrow=nrow,
            ncol=ncol,
            dimension=if (is.null(dimension)) ssm$ssl[[ssm$seriesCounter-1]]$dimension else dimension,
            slices=slices,
            begin=if (is.null(begin) && ssm$seriesCounter>1) ssm$ssl[[ssm$seriesCounter-1]]$begin else begin,
            end=if (is.null(end) && ssm$seriesCounter>1) ssm$ssl[[ssm$seriesCounter-1]]$end else end,
            rotate=if (is.null(rotate)) prev$rotate %||% 0 else rotate,
            flip=if (is.null(flip)) prev$flip %||% "none" else flip,
            seriesVP=NULL, #seriesVP,
            order=list(),
            legendInfo=list(),
            legendOrder=list(),
            sliceIndicator=NULL,
            title=list())
  class(l) <- c("sliceSeries", "list")
  ssm$ssl[[ssm$seriesCounter]] <- l

  return(ssm)
}

#' Compute bounding box of a mask volume
#'
#' Given a binary mask, returns the begin and end slices along the
#' specified dimension, with optional padding. Useful for setting the
#' \code{begin} and \code{end} arguments of \code{\link{sliceSeries}}.
#'
#' @param mask A 3D array where non-zero values indicate the region of interest.
#' @param dimension The dimension (1, 2, or 3) along which to compute bounds.
#' @param padding Number of voxels to add as padding on each side. Defaults to 10.
#'
#' @return A named numeric vector with elements \code{begin} and \code{end}.
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' whiteMatter <- v$labels == 1
#' bounds <- maskBounds(whiteMatter, dimension = 2, padding = 2)
#' bounds
#' sliceSeries(nrow = 1, ncol = 4, begin = bounds["begin"], end = bounds["end"]) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   draw()
maskBounds <- function(mask, dimension, padding = 10) {
  if (!is.array(mask) || length(dim(mask)) != 3)
    stop("'mask' must be a 3D array")
  if (!is.numeric(dimension) || length(dimension) != 1 || !(dimension %in% 1:3))
    stop("'dimension' must be 1, 2, or 3")
  inMask <- apply(mask != 0, dimension, any, na.rm = TRUE)
  if (!any(inMask))
    stop("mask contains no non-zero values")
  r <- range(which(inMask))
  c(begin = max(1, r[1] - padding), end = min(dim(mask)[dimension], r[2] + padding))
}

# fields of a slice series; a layer is stored next to them, so it cannot
# use one of these names
reservedLayerNames <- c("nrow", "ncol", "dimension", "slices", "begin", "end",
                        "seriesVP", "order", "legendInfo", "legendOrder",
                        "sliceIndicator", "title", "titleSide", "legendGlobal",
                        "anatomySpec", "rotate", "flip")

# The name under which a new layer is stored. A name that the user gives must
# be new in the series; with no name, use the default if it is free, otherwise
# make a unique one, so that a second layer never replaces the first.
layerName <- function(ssm, name, default) {
  ss <- getSS(ssm)
  used <- unlist(ss$order)
  if (is.null(name)) {
    if (!(default %in% used)) return(default)
    return(paste0(default, "#", ssm$seriesCounter, "_", length(ss$order) + 1))
  }
  if (!is.character(name) || length(name) != 1 || is.na(name) || name == "")
    stop("'name' must be one non-empty string")
  if (name %in% reservedLayerNames)
    stop("'", name, "' cannot be used as a layer name. Do not use these names: ",
         paste(reservedLayerNames, collapse = ", "))
  if (name %in% used)
    stop("A layer named '", name, "' is already in this slice series. ",
         "Give each layer a different name.")
  name
}

getSS <- function(ssm) {
  return(ssm$ssl[[ssm$seriesCounter]])
}
# Each pipe step returns a changed copy and leaves its input as it was, so a
# partial pipe can be stored and extended in more than one way. The slice
# series list is copied with the environment (R copies lists on change).
cloneSSM <- function(ssm) {
  list2env(as.list(ssm, all.names = TRUE), parent = globalenv())
}
putSS <- function(ssm, ss) {
  ssm <- cloneSSM(ssm)
  ssm$ssl[[ssm$seriesCounter]] <- ss
  ssm
}

makeSlices <- function(ss, volume) {
  validateVolume(volume)
  if (is.null(ss$seriesVP)) {
    sliceDims <- seriesSliceDims(ss, volume)
    gl <- grid.layout(nrow=ss$nrow, ncol=ss$ncol,
                      widths=rep(sliceDims[1], ss$ncol),
                      heights=rep(sliceDims[2], ss$nrow), respect = TRUE)
    ss$seriesVP <- viewport(layout = gl)
  }
  fromRange <- is.null(ss$slices)
  if (fromRange) {
    # the defaults are not stored in ss$begin/ss$end, so that a later series
    # inherits only what the user gave (a later series can have another
    # dimension, with another number of slices)
    d <- dim(volume)
    begin <- if (is.null(ss$begin)) 1 else ss$begin
    end <- if (is.null(ss$end)) d[ss$dimension] else ss$end
    nslices <- ss$nrow*ss$ncol
    ss$slices <- ceiling(seq(begin, end, length=nslices))
  }

  n <- dim(volume)[ss$dimension]
  s <- ss$slices
  bad <- is.na(s) | s < 1 | s > n | s != round(s)
  if (any(bad))
    stop("Slices must be whole numbers from 1 to ", n, " (dimension ", ss$dimension,
         " of the volume has ", n, " slices). Not valid: ",
         paste(unique(s[bad]), collapse = ", "),
         if (fromRange) ". Check 'begin' and 'end'." else ".")

  if (length(ss$slices) != ss$ncol*ss$nrow)
    warning("Number of slices does not equal nrow*ncol, likely resulting in weird figures")

  return(ss)
}


#' Add anatomy slices
#'
#' Adds the anatomy slices, or anything that can serve as an underlay.
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param volume 3D matrix representing the volume from which to obtain slices.
#'   Can be either numbers or hexadecimal colours
#' @param low Inclusive lower end of colour scale. If NULL, the 2nd percentile
#'   of the volume is used. Ignored if volume is a matrix of hexadecimals.
#' @param high Inclusive upper end of colour scale. If NULL, the 98th percentile
#'   of the volume is used. Ignored if volume is a matrix of hexadecimals.
#' @param col The colour scale. Defaults to gray.
#' @param alpha Value between 0 and 1, 0 being full transparent and 1 fully opaque.
#' @param name Optional name.
#'
#' @details If \code{volume} is NULL, the most recent earlier \code{anatomy()}
#'   call with a volume is repeated for the current slice series, with that
#'   call's volume, \code{low}, \code{high}, \code{col} and \code{alpha}. The
#'   slices, rows, columns and dimension come from the current series. Give
#'   \code{low}, \code{high}, \code{col} or \code{alpha} to change only that
#'   value.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   draw()
#'
#' # reuse the anatomy in a second series along another dimension
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   sliceSeries(dimension = 1) %>%
#'   anatomy() %>%
#'   draw()
anatomy <- function(ssm, volume=NULL, low=NULL, high=NULL,
                    col=gray.colors(255, start=0), alpha=NULL, name="anatomy") {
  # with no volume, repeat the most recent earlier anatomy call, but slice it
  # for the current series (its slices, rows, columns and dimension)
  if (is.null(volume)) {
    spec <- NULL
    for (i in rev(seq_len(ssm$seriesCounter - 1))) {
      spec <- ssm$ssl[[i]]$anatomySpec
      if (!is.null(spec)) break
    }
    if (is.null(spec)) stop("A volume must be specified the first time anatomy is used")
    volume <- spec$volume
    if (missing(low)) low <- spec$low
    if (missing(high)) high <- spec$high
    if (missing(col)) col <- spec$col
    if (missing(alpha)) alpha <- spec$alpha
  }
  name <- layerName(ssm, if (missing(name)) NULL else name, "anatomy")
  ssm <- slice(ssm, volume, low, high, col=col, alpha=alpha, name=name)
  ss <- getSS(ssm)
  ss$anatomySpec <- list(volume=volume, low=low, high=high, col=col, alpha=alpha)
  putSS(ssm, ss)
}


#' Add overlay slices
#'
#' Adds overlay slices; usually used for statistics, but can be anything.
#' Differs from anatomy in that the background is transparent.
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param volume 3D matrix representing the volume from which to obtain slices.
#'   Can be either numbers or hexadecimal colours
#' @param low Inclusive lower end of colour scale. If symmetric=TRUE, then treat
#'   this as an absolute value (i.e. 2 would be >2 and < -2). If NULL, the 2nd
#'   percentile of the volume (of its absolute values if symmetric=TRUE) is
#'   used. Ignored if volume is a matrix of hexadecimals.
#' @param high Inclusive upper end of colour scale. If NULL, the 98th percentile
#'   of the volume (of its absolute values if symmetric=TRUE) is used.
#' @param col The colour scale. Defaults to \code{\link{defaultCol}()}, a
#'   red-yellow ramp that can be changed with \code{options(MRIcrotomeCol = ...)}.
#' @param symmetric Whether the colour scale is symmetric.
#' @param rCol The reverse colour scale. Used if symmetric=TRUE. Defaults to
#'   \code{\link{defaultRCol}()}, a blue-turquoise ramp that can be changed
#'   with \code{options(MRIcrotomeRcol = ...)}.
#' @param alpha Value between 0 and 1, 0 being full transparent and 1 fully opaque.
#' @param underTransparent Whether to make the under colour transparent. Defaults to TRUE.
#' @param name Optional name.
#' @param box Whether to draw a box around the overlay. Defaults to FALSE.
#' @param interpolate Logical; whether to apply linear interpolation to the
#'   overlay image when drawing. Set to \code{FALSE} for sharp boundaries
#'   (e.g., label overlays with discrete colours). Defaults to TRUE.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics") %>%
#'   draw()
#'
#' # labels as an overlay, with sharp edges
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$labels, low = 1, high = 2, col = c("blue", "orange"),
#'           alpha = 0.4, interpolate = FALSE) %>%
#'   draw()
overlay <- function(ssm, volume, low=NULL, high=NULL, col=defaultCol(),
                    symmetric=FALSE, rCol=defaultRCol(), alpha=NULL,
                    underTransparent = TRUE, name=NULL, box=FALSE,
                    interpolate = TRUE) {

  if (is.null(name)) {
    ss <- getSS(ssm)
    name <- paste0("overlay#", ssm$seriesCounter, "_", length(ss$order) + 1)
  }
  name <- layerName(ssm, name, name)
  slice(ssm, volume, low, high, col=col, name=name, underTransparent = underTransparent, symmetric = symmetric,
        rCol=rCol, alpha=alpha, box=box, interpolate = interpolate)
}


#' Add a title to a slice series
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param title The text that will be used for the title.
#' @param side Which side of the slices to put the title on in row layout
#'   (\code{draw(layout = "row")}): \code{"right"} (default) or \code{"left"}.
#'   Ignored in column layout, where titles go above the slices.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' sliceSeries(nrow = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   addtitle("Anatomy") %>%
#'   sliceSeries() %>%
#'   anatomy() %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   addtitle("Stats") %>%
#'   legend("t-statistics") %>%
#'   draw()
addtitle <- function(ssm, title, side = "right") {
  side <- match.arg(side, c("left", "right"))
  ss <- getSS(ssm)
  ss$title <- title
  ss$titleSide <- side
  ssm <- putSS(ssm, ss)
  return(ssm)
}

#' Add a legend
#'
#' Adds a legend based on the current slice series. Colour bars, contours, etc.,
#' are taken from the immediately preceding element. A second \code{legend()}
#' call for the same element replaces its description and \code{digits}; it
#' does not add a second legend.
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param description The text that will be used to describe the legend. Can be
#'   NULL, in which case the colour bar is shown without a description.
#' @param global If TRUE, and the figure is drawn with \code{layout = "row"},
#'   the legend spans all rows as one shared legend for the whole figure
#'   instead of sitting beside its own slice series. In column layout a legend
#'   already spans the full height, so this has no effect there. All legends
#'   of a slice series share one side panel, so if any \code{legend()} call of
#'   the series has \code{global = TRUE}, the whole panel is global.
#' @param digits The smallest number of significant digits in the numbers of a
#'   colour bar. The numbers are rounded to this, but the whole-number part is
#'   never rounded (so 1400 stays 1400). Defaults to 3.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' sliceSeries(nrow = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   addtitle("Anatomy") %>%
#'   sliceSeries() %>%
#'   anatomy() %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   addtitle("Stats") %>%
#'   legend("t-statistics") %>%
#'   draw()
#'
#' # one colour bar for three views drawn as rows
#' sliceSeries(nrow = 1, ncol = 5, begin = 10, end = 40, dimension = 2) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   sliceSeries(dimension = 1) %>%
#'   anatomy() %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   sliceSeries(dimension = 3) %>%
#'   anatomy() %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics", global = TRUE) %>%
#'   draw(layout = "row")
legend <- function(ssm, description=NULL, global=FALSE, digits=3) {
  ss <- getSS(ssm)
  if (length(ss$order) == 0)
    stop("legend() must come after a layer (anatomy(), overlay() or contours()) ",
         "in the same slice series")
  last <- ss$order[[length(ss$order)]]
  # a second legend() on the same layer changes that legend, not adds one
  if (!(last %in% unlist(ss$legendOrder))) ss$legendOrder <- c(ss$legendOrder, last)
  ss[["legendInfo"]][[last]]$description = description
  ss[["legendInfo"]][[last]]$digits = digits
  # the side panel of a series is either global or not; once one of its
  # legends asks for global, it stays global
  ss$legendGlobal <- isTRUE(ss$legendGlobal) || isTRUE(global)
  ssm <- putSS(ssm, ss)
  return(ssm)
}

slice <- function(ssm, volume, low, high, col, underTransparent = FALSE, symmetric=FALSE,
                  rCol=defaultRCol(), alpha=NULL,name=NULL, box=FALSE, interpolate = TRUE) {
  ss <- getSS(ssm)
  ss <- makeSlices(ss, volume)
  # resolve the colour range once per volume so every slice and the legend agree
  if (!is.character(volume)) {
    r <- getRange(if (symmetric) abs(volume) else volume, low, high)
    low <- r[1]; high <- r[2]
    if (low == high)
      warning("low and high are both ", low, ", so the colour scale has only one ",
              "value: voxels at or above it get the first colour of the scale.",
              call. = FALSE)
  }
  sliceList <- list()
  counter <- 1
  sliceDims <- seriesSliceDims(ss, volume)
  for (i in 1:ss$nrow) {
    for (j in 1:ss$ncol) {
      sliceList[[counter]] <- sliceImage(volume, ss$dimension,low,high,col=col,
                                         slice=ss$slices[counter],
                                         rotate = ss$rotate %||% 0,
                                         flip = ss$flip %||% "none",
                                         underTransparent = underTransparent,
                                         symmetric = symmetric,
                                         rCol = rCol, alpha=alpha, box=box,
                                         interpolate = interpolate,
                                         vp=viewport(layout.pos.row = i,
                                                     layout.pos.col = j,
                                                     xscale=c(0, sliceDims[1]),
                                                     yscale=c(0, sliceDims[2])))
      counter <- counter+1
    }
  }
  ss[[name]] <- sliceList
  ss[["legendInfo"]][[name]] <- list(type="slice", low=low, high=high, col=col, rCol=rCol, symmetric=symmetric)
  ss[["order"]][[length(ss$order)+1]] <- name
  ssm <- putSS(ssm, ss)
  return(ssm)
}


#' Add contours to a slice series
#'
#' Adds contours to every slice in the series, with control over the contour
#' levels and attributes.
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param volume 3D matrix representing the volume from which to obtain contours
#' @param levels A vector of levels at which to draw the contours. Give either
#'   \code{levels} or \code{labels}, not both.
#' @param col The colour for the contours. Can be a single value, in which case
#'   all levels will use the same colour, or a vector if different colours for
#'   different levels are desired. Default is red.
#' @param lty The line type for the contours. Can be a single value, in which case
#'   all levels will use the same line type, or a vector if different line types for
#'   different levels are desired. Default is 1.
#' @param lwd The line width for the contours. Can be a single value, in which case
#'   all levels will use the same line width, or a vector if different line widths for
#'   different levels are desired. Default is 1.
#' @param name An optional name. It must be different from the names of the
#'   other layers in the slice series. With no name, the first contours layer
#'   of a series is named \code{"contours"} and each later one gets a unique
#'   name.
#' @param labels An optional vector of label values to contour, for atlas or
#'   segmentation volumes. Voxels whose value is in \code{labels} are turned
#'   into a binary mask and the contour is drawn at its boundary. Give either
#'   \code{levels} or \code{labels}, not both.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics") %>%
#'   contours(abs(v$stats), levels = c(3, 5), lwd = 2, lty = c(3, 1), col = "green") %>%
#'   legend("|t|") %>%
#'   draw()
#'
#' # outline the white matter from a volume of labels
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   contours(v$labels, labels = 1, col = "magenta") %>%
#'   legend("white matter") %>%
#'   draw()
contours <- function(ssm, volume, levels=NULL, col="red", lty=1, lwd=1, name="contours", labels=NULL) {
  if (!is.null(labels) && !is.null(levels)) stop("Cannot specify both 'levels' and 'labels'")
  if (is.null(labels) && is.null(levels)) stop("Must specify either 'levels' or 'labels'")
  name <- layerName(ssm, if (missing(name)) NULL else name, "contours")
  ss <- getSS(ssm)
  ss <- makeSlices(ss, volume)   # checks the volume before it is used below
  if (!is.null(labels)) {
    volume <- array(as.integer(volume %in% labels), dim = dim(volume))
    levels <- 0.5
  }
  sliceList <- list()
  counter <- 1
  sliceDims <- seriesSliceDims(ss, volume)
  for (i in 1:ss$nrow) {
    for (j in 1:ss$ncol) {
      sliceList[[counter]] <- sliceContours(volume, ss$dimension,levels = levels, col=col, lty=lty, lwd=lwd,
                                         slice=ss$slices[counter],
                                         rotate = ss$rotate %||% 0,
                                         flip = ss$flip %||% "none",
                                         vp=viewport(layout.pos.row = i,
                                                     layout.pos.col = j,
                                                     xscale=c(0, sliceDims[1]),
                                                     yscale=c(0, sliceDims[2])))
      counter <- counter+1
    }
  }
  ss[[name]] <- sliceList
  ss[["order"]][[length(ss$order)+1]] <- name
  ss[["legendInfo"]][[name]] <- list(type="contour", levels=levels, col=col, lty=lty, lwd=lwd,
                                     labels=labels)
  ssm <- putSS(ssm, ss)
  return(ssm)
}


#' Add a slice indicator
#'
#' Adds an indicator of the slices used in the slice series, displayed on top of
#' an anatomy slice.
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param volume 3D matrix representing the volume from which to draw the
#'   background slice.
#' @param low Inclusive low end of colour for background slice.
#' @param high Inclusive high end of colour for background slice.
#' @param dimension Dimension to use for background slice. Will pick sensible
#'   default if NULL.
#' @param slice Slice number to use for background slice. Will pick sensible
#'   default if NULL.
#' @param col Colour of the background slice. Defaults to gray.
#' @param lineColour The colour of the slice indicator lines. Defaults to green.
#'
#' @return The ssm object augmented with a sliceIndicator
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics") %>%
#'   anatomySliceIndicator(v$anatomy, 600, 1400) %>%
#'   draw()
anatomySliceIndicator <- function(ssm, volume, low, high, dimension=NULL,
                                  slice=NULL, col=gray.colors(255, start=0),
                                  lineColour="green") {
  validateVolume(volume)
  defDS <- sliceIndicatorDefaultDimensionAndSlice(ssm, volume)
  if (is.null(dimension)) dimension <- defDS[[1]]
  if (is.null(slice)) slice <- defDS[[2]]

  indVP <- setupSliceIndicatorVP(volume, dimension)
  indSlice <- sliceImage(volume, dimension,slice, low, high, col=col, vp=indVP)
  return(sliceIndicator(ssm, volume, dimension, indSlice, indVP, lineColour))
}


#' Add a slice indicator
#'
#' Adds an indicator of the slices used in the slice series, displayed on top of
#' one or more contours.
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param volume 3D matrix representing the volume from which to draw the
#'   contours.
#' @param levels Levels at which to draw contours - a single number or a vector.
#' @param dimension Dimension to use for contours. Will pick sensible
#'   default if NULL.
#' @param slice Slice number to use for contours. Will pick sensible
#'   default if NULL.
#' @param col The colour(s) for the contour(s). Defaults to red.
#' @param lty The line type(s) for the contour(s). Defaults to 1.
#' @param lwd The line width(s) for the contour(s). Defaults to 1.
#' @param lineColour The colour for the slice indicator lines. Defaults to black.
#'
#' @return The ssm augmented with a contour slice indication
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics") %>%
#'   contourSliceIndicator(v$anatomy, levels = 1030, col = "gray30") %>%
#'   draw()
contourSliceIndicator <- function(ssm, volume, levels, dimension=NULL, slice=NULL, col="red", lty=1, lwd=1, lineColour="black") {
  validateVolume(volume)
  defDS <- sliceIndicatorDefaultDimensionAndSlice(ssm, volume)
  if (is.null(dimension)) dimension <- defDS[[1]]
  if (is.null(slice)) slice <- defDS[[2]]


  indVP <- setupSliceIndicatorVP(volume, dimension)
  indContour <- sliceContours(volume, dimension,levels = levels, col=col, lty=lty, lwd=lwd,
                              slice=slice, vp=indVP)
  return(sliceIndicator(ssm, volume, dimension, indContour, indVP, lineColour))
}

setupSliceIndicatorVP <- function(volume, dimension) {
  sliceDims <- dim(volume)[-dimension]
  indVP <- viewport(layout.pos.row = 1,
                    layout.pos.col = 1,
                    xscale = c(0, sliceDims[1]),
                    yscale = c(0, sliceDims[2]))
  return(indVP)
}

sliceIndicatorDefaultDimensionAndSlice <- function(ssm, volume) {
  ss <- getSS(ssm)
  if (ss$dimension %in% c(2,3)) {
    d <- 1
  } else {
    d <- 2
  }

  dims <- dim(volume)
  s <- ceiling(dims[d]/2)
  return(list(d,s))

}

sliceIndicator <- function(ssm, volume, dimension, bgGrob, indVP, lineColour="green") {
  ss <- getSS(ssm)
  # an indicator can come before the first layer; the slices are needed here
  if (is.null(ss$slices)) ss <- makeSlices(ss, volume)
  if (dimension == ss$dimension) {
    stop("sliceIndicator dimension cannot be equal to sliceSeries dimension")
  }
  sliceDims <- dim(volume)[-dimension]
  overVP <- viewport(layout = grid.layout(1, 1, widths = sliceDims[1],
                                          heights = sliceDims[2],
                                          respect = TRUE))
  lineList <- list()
  counter <- 1
  for (i in 1:ss$nrow) {
    for (j in 1:ss$ncol) {
      # a line through the centre of the slice's voxels (slice s fills native
      # coordinates s-1 to s), on the same side as the flipped image
      if ( (ss$dimension == 2 && dimension == 1) ||
           (ss$dimension == 1) ) {
        xd <- c(rep(ss$slices[counter] - 0.5, 2))
        yd <- c(0,sliceDims[2])
      } else {
        xd <- c(0, sliceDims[1])
        yd <- flipNativeY(c(rep(ss$slices[counter] - 0.5, 2)), sliceDims[2])
      }
      lineList[[counter]] <- linesGrob(x = xd,
                                       y = yd,
                                       default.units = "native",
                                       vp=indVP, gp=gpar(col=lineColour))
      counter <- counter+1
    }
  }
  ss[["sliceIndicator"]] <- gTree(vp=overVP, children=do.call(gList, c(bgGrob, lineList)))
  ssm <- putSS(ssm, ss)
  return(ssm)
}

assembleLegends <- function(ss, gp = gpar()) {

  if (length(ss$legendOrder) == 0) return(NULL)

  nrows <- (length(ss$legendOrder)*2)-1
  widthSize <- c(rbind(rep(1, length(ss$legendOrder)),
                       rep(0.2, length(ss$legendOrder))))

  widthUnits <- c(rbind(rep("null", length(ss$legendOrder)),
                        rep("lines", length(ss$legendOrder))))

  widthSize <- widthSize[1:(length(widthSize)-1)]
  widthUnits <- widthUnits[1:(length(widthUnits)-1)]

  legendViewport <- viewport(layout=grid.layout(nrow=nrows,
                                                ncol=1, heights=unit(widthSize, widthUnits)))
  legendGrobs <- list()
  for (i in seq_along(ss$legendOrder)) {
    li <- ss$legendInfo[[ss$legendOrder[[i]]]]
    if (li$type == "slice")
      gl <- gList(sliceLegendGrob(li$low, li$high, li$col,
                                  li$rCol, li$symmetric,
                                  colWidth = unit(1, "lines"),
                                  description = li$description, gp = gp,
                                  digits = li$digits %||% 3))
    else if (li$type == "contour")
      gl <- gList(contourLegendGrob(levels=contourLegendLevels(li), col=li$col, lty=li$lty, lwd=li$lwd,
                                    description=li$description, gp = gp))

    legendGrobs[[i]] <- gTree(vp=viewport(layout.pos.row = (i*2)-1, x=1),
                              children=gl)
  }

  gTree(vp=legendViewport, children=do.call(gList, legendGrobs))
}

hasSidePanel <- function(ss) length(ss$legendOrder) > 0 || !is.null(ss$sliceIndicator)

# The side panel next to a slice series: the slice indicator in the top 15% and
# the legends underneath. Either part can be absent.
assembleSidePanel <- function(ss, gp = gpar()) {
  legends <- assembleLegends(ss, gp)
  if (is.null(ss$sliceIndicator)) return(legends)
  children <- list(gTree(vp=viewport(layout.pos.row = 1), children=gList(ss$sliceIndicator)))
  if (!is.null(legends))
    children <- c(children, list(gTree(vp=viewport(layout.pos.row = 2), children=gList(legends))))
  gTree(vp=viewport(layout=grid.layout(nrow=2, ncol=1, heights=unit(c(0.15, 0.85), "null"))),
        children=do.call(gList, children))
}

grobifySliceSeries <- function(ss, sliceBg = "black") {
  # the background behind the slices; NULL or NA for none
  grobList <- if (is.null(sliceBg) || is.na(sliceBg)) list()
              else list(rectGrob(gp=gpar(fill=sliceBg)))

  for (layer in ss$order) {
    grobList <- c(grobList, ss[[layer]])
  }

  gTree(children=do.call(gList, grobList), vp=ss$seriesVP)
}

#' Turn the slice series into grobs
#'
#' Takes the slice series, creates grobs where necessary, and returns a gTree.
#' Can then be incorporated into other grid functions.
#'
#' @param ssm The slice series
#' @param layout Whether to lay out by \code{"column"} (the default) or by
#'   \code{"row"}.
#' @param titlePars A `gpar` list configuring text grobs when generating titles.
#' useful for setting e.g. `fontsize` and `col`
#' @param legendPars A `gpar` list configuring text grobs when generating legends
#' useful for setting e.g. `fontsize` and `col`
#' @param bgCol Either NULL or a string indicating what colour the figure background
#' should be.
#' @param sliceBg The colour of the rectangle behind the slices of each slice
#'   series. Defaults to \code{"black"}. Use \code{NA} or \code{NULL} for no
#'   rectangle, so that \code{bgCol} (or the page) shows around and through
#'   the slices.
#' 
#' @return A gTree object with the figure, to be drawn with [grid.draw]
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' g <- sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics") %>%
#'   anatomySliceIndicator(v$anatomy, 600, 1400) %>%
#'   grobify()
#' grid::grid.newpage()
#' grid::grid.draw(g)
grobify <- function(ssm, layout="column", titlePars = gpar(), legendPars = gpar(), bgCol = NULL,
                    sliceBg = "black") {
  for (i in seq_along(ssm$ssl)) {
    if (length(ssm$ssl[[i]]$order) == 0)
      stop("Slice series ", i, " has no layers. Add anatomy(), overlay() or ",
           "contours() after sliceSeries().")
  }
  layout <- match.arg(layout, c("column", "row"))
  if (layout=="column") grobifyByColumn(ssm, titlePars, legendPars, bgCol, sliceBg)
  else grobifyByRow(ssm, titlePars, legendPars, bgCol, sliceBg)
}

grobifyByRow <- function(ssm, titlePars = gpar(), legendPars = gpar(), bgCol = NULL,
                         sliceBg = "black") {
  nseries <- length(ssm$ssl)
  sides <- sapply(ssm$ssl, function(x) if (length(x$title) > 0) x$titleSide %||% "right" else NA_character_)
  haveLeft <- any(sides == "left", na.rm = TRUE)
  haveRight <- any(sides == "right", na.rm = TRUE)
  haveLegends <- any(sapply(ssm$ssl, hasSidePanel))

  # columns, each present only when some series needs it: [left title] slices [right title] [legend]
  sliceCol <- 1 + haveLeft
  rightCol <- sliceCol + 1
  legendCol <- sliceCol + haveRight + 1
  ncol <- sliceCol + haveRight + haveLegends
  widths <- c(if (haveLeft) 0.1,
              1 - 0.1 * (haveLeft + haveRight) - 0.2 * haveLegends,
              if (haveRight) 0.1,
              if (haveLegends) 0.2)

  gs <- list()
  for (i in 1:nseries) {
    ss <- ssm$ssl[[i]]
    gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.row = i, layout.pos.col = sliceCol),
                                children=gList(grobifySliceSeries(ss, sliceBg)))
    if (!is.na(sides[i])) {
      gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.row = i,
                                              layout.pos.col = if (sides[i] == "left") 1 else rightCol),
                                  children=gList(textGrob(ss$title, rot=90, gp = titlePars)))
    }
    if (hasSidePanel(ss)) {
      # a global legend spans every row of the figure
      rows <- if (isTRUE(ss$legendGlobal)) 1:nseries else i
      gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.row = rows, layout.pos.col = legendCol),
                                  children=gList(assembleSidePanel(ss, gp = legendPars)))
    }
  }

  if(!is.null(bgCol))
    gs <- c(list(rectGrob(gp = gpar(col = NA, fill = bgCol))), gs)

  vA <- viewport(layout = grid.layout(nseries, ncol, widths = widths))

  return(gTree(children=do.call(gList, gs), vp=vA))
}

grobifyByColumn <- function(ssm, titlePars = gpar(), legendPars = gpar(), bgCol = NULL,
                            sliceBg = "black") {
  nseries <- length(ssm$ssl)

  gs <- list()
  widths = list()


  # allow for an extra row if there are any titles present
  haveTitles <- any(sapply(ssm$ssl, function(x) length(x$title))>0)
  row <- 1
  heights <- unit(1, "null")
  if (haveTitles) {
    row <- 2
    heights <- unit.c(unit(1, "lines"), heights)
  }

  # go through all sliceSeries
  j <- 1 # counter of which column is being worked on
  for (i in 1:nseries) {
    # add a title as a textGrob if title is present
    if (length(ssm$ssl[[i]]$title) > 0) {
      gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.col = j, layout.pos.row = 1),
                                  children=gList(textGrob(ssm$ssl[[i]]$title, gp = titlePars)))
    }
    # add the slices
    gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.col = j, layout.pos.row = row),
                     children=gList(grobifySliceSeries(ssm$ssl[[i]], sliceBg)))
    widths[[j]] <- unit(1, "null")
    j <- j+1
    # add the legend(s) and/or indicator if present
    if (hasSidePanel(ssm$ssl[[i]])) {
      gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.col = j, layout.pos.row = row),
                       children=gList(assembleSidePanel(ssm$ssl[[i]], gp = legendPars)))
      widths[[j]] <- legendWidth(ssm$ssl[[i]], legendPars)
      j <- j+1
    }
  }

  if(!is.null(bgCol))
      gs <- c(list(rectGrob(gp = gpar(col = NA, fill = bgCol))), gs)
  
  # create the viewport and assemble the list of grobs
  vA <- viewport(layout = grid.layout(row, j-1,
                                      widths = do.call(unit.c, widths),
                                      heights=heights))
  return(gTree(children=do.call(gList, gs), vp=vA))

}

#' Draw a slice series figure
#'
#' Renders the slice series to the current graphics device. Opens a new page and
#' draws the assembled figure including all slice series, overlays, contours,
#' legends, and titles.
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param layout Either \code{"column"} (the default) or \code{"row"}: how to
#'   lay out the figure.
#' @param ... Other arguments for \code{\link{grobify}}, such as
#'   \code{titlePars}, \code{legendPars}, \code{bgCol} and \code{sliceBg}.
#'
#' @return invisible NULL
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics") %>%
#'   draw()
#'
#' # a white page with no black rectangle behind the slices
#' sliceSeries(nrow = 1, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   draw(sliceBg = NA)
draw <- function(ssm, layout="column", ...) {
  grid.newpage()
  # a small margin so that labels at the figure edge are not flush with the page
  grid.draw(gTree(children=gList(grobify(ssm, layout=layout, ...)),
                  vp=viewport(width = 0.95, height = 0.95)))
  invisible(NULL)
}
