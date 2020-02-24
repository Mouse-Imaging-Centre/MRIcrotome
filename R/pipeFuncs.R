initMetaSliceSeries <- function() {
  ssm <- new.env(parent = globalenv())
  ssm$seriesCounter=1
  ssm$ssl=list()
  return(ssm)
}


#' Initialize a slice series
#'
#' Starts a new slices series, sets dimensions, slices to use, and rows and
#' columns.
#'
#' @param ssm Provided as part of an ongoing pipe, not usually specified by user
#' @param slices A vector of integers representing which slices to use. Can be
#'   NULL, in which case nrow and ncol together with begin and end are used.
#' @param nrow Rows in the slice series. Set to 1 if NULL and ncol specified.
#' @param ncol Columns in the slice series. Set to 1 if NULL and nrow specified.
#' @param dimension Dimension as integer (1, 2, or 3) for slice series.
#' @param begin First slice. Can be NULL if slices explicitly specified.
#' @param end Last slice. Can be NULL if slices explicitly specified.
#'
#' @return Returns the slices series info for passing along the pipe.
#' @export
#'
#' @examples
#' \dontrun{
#' sliceSeries(nrow = 1, begin=200, end=300) %>%
#'   anatomy(anatVol, low=700, high=1400) %>%
#'   overlay(stats, low=2, high=6, symmetric = T) %>%
#'   legend("t-statistics") %>%
#'   draw()
#' }
sliceSeries <- function(ssm=NULL,
                        slices=NULL,
                        nrow=NULL,
                        ncol=NULL,
                        dimension=NULL,
                        begin=NULL,
                        end=NULL) {

  # ssm (the first arg) is provided if this is part of an ongoing pipe, but should be
  # null if this is the first initialization
  if (is.null(ssm)) {
    ssm <- initMetaSliceSeries()
    # it is the first time sliceSeries was called in a pipe,
    # so check that necessary args are present
    if (is.null(nrow) & is.null(ncol)) stop("Must specify either nrow or ncol or both")

    # if only one of nrow or ncol is specified, set the other to 1
    if (is.null(nrow)) nrow <- 1
    if (is.null(ncol)) ncol <- 1

    # use a default dimension of 2
    if (is.null(dimension)) dimension <- 2

  }
  else {
    ssm$seriesCounter = ssm$seriesCounter+1
  }
  # add checking for slices vs nrow and ncol

  nrow <- ifelse(is.null(nrow), ssm$ssl[[ssm$seriesCounter-1]]$nrow, nrow)
  ncol <- ifelse(is.null(ncol), ssm$ssl[[ssm$seriesCounter-1]]$ncol, ncol)

  # initialize sliceSeries list; in all cases use the previous sliceSeries' values
  # if they were not specified here
  l <- list(nrow=nrow,
            ncol=ncol,
            dimension=ifelse(is.null(dimension), ssm$ssl[[ssm$seriesCounter-1]]$dimension, dimension),
            slices=slices, #if(is.null(slices) & (ssm$seriesCounter>1)) ssm$ssl[[ssm$seriesCounter-1]]$slices else slices,
            #slices=if (is.null(slices) & ssm$seriesCounter>1) {ssm$ssl[[ssm$seriesCounter-1]]} else slices,
            begin=if (is.null(begin) & ssm$seriesCounter>1) {ssm$ssl[[ssm$seriesCounter-1]]$begin} else {begin },
            end=if (is.null(end) & ssm$seriesCounter>1) {ssm$ssl[[ssm$seriesCounter-1]]$end} else {end},
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

getSS <- function(ssm) {
  return(ssm$ssl[[ssm$seriesCounter]])
}
putSS <- function(ssm, ss) {
  ssm$ssl[[ssm$seriesCounter]] <- ss
}

makeSlices <- function(ss, volume) {
  #message("seriesVP", ss$seriesVP)
  #message("slices", ss$slices)
  if (is.null(ss$seriesVP)) {
    sliceDims <- dim(volume)[-ss$dimension]
    gl <- grid.layout(nrow=ss$nrow, ncol=ss$ncol,
                      widths=rep(sliceDims[1], ss$ncol),
                      heights=rep(sliceDims[2], ss$nrow), respect = TRUE)
    ss$seriesVP <- viewport(layout = gl)
  }
  if (is.null(ss$slices)) {
    d <- dim(volume)
    if (is.null(ss$begin)) ss$begin <- 1
    if (is.null(ss$end)) ss$end <- d[ss$dimension]
    nslices <- ss$nrow*ss$ncol
    ss$slices <- ceiling(seq(ss$begin, ss$end, length=nslices))
  }

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
#' @param low Lower end of colour scale. Can be NULL if volume is a matrix of hexadecimals.
#' @param high Upper end of colour scale. Can be NULL if volume is a matrix of hexadecimals.
#' @param col The colour scale. Defaults to gray.
#' @param alpha Value between 0 and 1, 0 being full transparent and 1 fully opaque.
#' @param name Optional name.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' \dontrun{
#' sliceSeries(nrow = 1, begin=200, end=300) %>%
#'   anatomy(anatVol, low=700, high=1400) %>%
#'   overlay(stats, low=2, high=6, symmetric = T) %>%
#'   legend("t-statistics") %>%
#'   draw()
#' }
anatomy <- function(ssm, volume=NULL, low=NULL, high=NULL,
                    col=gray.colors(255, start=0), alpha=NULL, name="anatomy") {
  # if there is no volume specified, then reuse the previous sliceSeries' anatomy
  if (is.null(volume)) {
    if (ssm$seriesCounter == 1) stop("A volume must be specified the first time anatomy is used")
    ss <- getSS(ssm)
    ss[[name]] <- ssm$ssl[[ssm$seriesCounter-1]][["anatomy"]]
    ss[["seriesVP"]] <- ssm$ssl[[ssm$seriesCounter-1]][["seriesVP"]]
    ss[["legendInfo"]][[name]] <-
      list(type="slice",
           low=ssm$ssl[[ssm$seriesCounter-1]]$low,
           high=ssm$ssl[[ssm$seriesCounter-1]]$high,
           col=ssm$ssl[[ssm$seriesCounter-1]]$col)
    ss[["order"]][[length(ss$order)+1]] <- name
    putSS(ssm, ss)
    return(ssm)
  } else {
    slice(ssm, volume, low, high, col=col, alpha=alpha, name=name)
  }
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
#' @param low Lower end of colour scale. Can be NULL if volume is a matrix of
#'   hexadecimals. If symmetric=TRUE, then treat this as an absolute value (i.e.
#'   2 would be >2 and < -2)
#' @param high Upper end of colour scale.
#' @param col The colour scale. See defaultCol for default.
#' @param symmetric Whether the colour scale is symmetric.
#' @param rCol The reverse colour scale. Used if symmetric=TRUE. See defaultRCol
#'   for default.
#' @param alpha Value between 0 and 1, 0 being full transparent and 1 fully opaque.
#' @param underTransparent Whether to make the under colour transparent. Defaults to TRUE.
#' @param name Optional name.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' \dontrun{
#' sliceSeries(nrow = 1, begin=200, end=300) %>%
#'   anatomy(anatVol, low=700, high=1400) %>%
#'   overlay(stats, low=2, high=6, symmetric = T) %>%
#'   legend("t-statistics") %>%
#'   draw()
#' }
overlay <- function(ssm, volume, low=NULL, high=NULL, col=defaultCol(),
                    symmetric=FALSE, rCol=defaultRCol(), alpha=NULL,
                    underTransparent = TRUE, name=NULL, box=FALSE) {

  if (is.null(name)) name <- paste0("overlay#", ssm$seriesCounter)
  slice(ssm, volume, low, high, col=col, name=name, underTransparent = underTransparent, symmetric = symmetric,
        rCol=rCol, alpha=alpha, box=box)
}


#' Add a title to a slice series
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param title The text that will be used for the title.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' \dontrun{
#' sliceSeries(nrow = 8, begin=100, end=350) %>%
#'   anatomy(anatVol, low=700, high=1400) %>%
#'   addtitle("Anatomy") %>%
#'   sliceSeries() %>%
#'   anatomy() %>%
#'   overlay(stats, low=2, high=6, symmetric = T) %>%
#'   addtitle("Stats") %>%
#'   legend("t-statistics") %>%
#'   draw()
#' }
addtitle <- function(ssm, title) {
  ss <- getSS(ssm)
  ss$title <- title
  putSS(ssm, ss)
  return(ssm)
}

#' Add a legend
#'
#' Adds a legend based on the current slice series. Colour bars, contours, etc.,
#' are taken from the immediately preceding element.
#'
#' @param ssm The slice series info, usually passed along the pipe and specified
#'   by the user
#' @param description The text that will be used to describe the legend. Can be
#'   NULL, in which case the colour bar is shown without a description.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' \dontrun{
#' sliceSeries(nrow = 8, begin=100, end=350) %>%
#'   anatomy(anatVol, low=700, high=1400) %>%
#'   addtitle("Anatomy") %>%
#'   sliceSeries() %>%
#'   anatomy() %>%
#'   overlay(stats, low=2, high=6, symmetric = T) %>%
#'   addtitle("Stats") %>%
#'   legend("t-statistics") %>%
#'   draw()
#' }
legend <- function(ssm, description=NULL) {
  ss <- getSS(ssm)
  ss$legendOrder <- c(ss$legendOrder, ss$order[[length(ss$order)]])
  ss[["legendInfo"]][[length(ss$order)]]$description = description
  putSS(ssm, ss)
  return(ssm)
}

slice <- function(ssm, volume, low, high, col,reverse = FALSE, underTransparent = FALSE, symmetric=FALSE,
                  rCol=defaultRCol(), alpha=NULL,name=NULL, box=FALSE) {
  ss <- getSS(ssm)
  ss <- makeSlices(ss, volume)
  #message(paste(ss$slices, collapse = " "))
  sliceList <- list()
  counter <- 1
  sliceDims <- dim(volume)[-ss$dimension]
  for (i in 1:ss$nrow) {
    for (j in 1:ss$ncol) {
      sliceList[[counter]] <- sliceImage(volume, ss$dimension,low,high,col=col,
                                         slice=ss$slices[counter],
                                         underTransparent = underTransparent,
                                         symmetric = symmetric,
                                         rCol = rCol, alpha=alpha, box=box,
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
  putSS(ssm, ss)
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
#' @param levels A vector of levels at which to draw the contours.
#' @param col The colour for the contours. Can be a single value, in which case
#'   all levels will use the same colour, or a vector if different colours for
#'   different levels are desired. Default is red.
#' @param lty The line type for the contours. Can be a single value, in which case
#'   all levels will use the same line type, or a vector if different line types for
#'   different levels are desired. Default is 1.
#' @param lwd The line width for the contours. Can be a single value, in which case
#'   all levels will use the same line width, or a vector if different line widths for
#'   different levels are desired. Default is 1.
#' @param name An optional name.
#'
#' @return The slices series for continuation down the pipe.
#' @export
#'
#' @examples
#' \dontrun{
#' sliceSeries(nrow = 1, begin=200, end=300) %>%
#'   anatomy(anatVol, low=700, high=1400) %>%
#'   overlay(stats, low=2, high=6, symmetric = T) %>%
#'   legend("t-statistics") %>%
#'   contours(abs(stats), levels=c(3,5), lwd=2, lty=c(3,1), col="green") %>%
#'   draw()
#' }
contours <- function(ssm, volume, levels, col="red", lty=1, lwd=1, name="contours") {
  ss <- getSS(ssm)
  ss <- makeSlices(ss, volume)
  sliceList <- list()
  counter <- 1
  sliceDims <- dim(volume)[-ss$dimension]
  for (i in 1:ss$nrow) {
    for (j in 1:ss$ncol) {
      sliceList[[counter]] <- sliceContours(volume, ss$dimension,levels = levels, col=col, lty=lty, lwd=lwd,
                                         slice=ss$slices[counter],
                                         vp=viewport(layout.pos.row = i,
                                                     layout.pos.col = j,
                                                     xscale=c(0, sliceDims[1]),
                                                     yscale=c(0, sliceDims[2])))
      counter <- counter+1
    }
  }
  ss[[name]] <- sliceList
  #ss[["legendInfo"]][[name]] <- list(low=low, high=high, col=col, rCol=rCol, symmetric=symmetric)
  ss[["order"]][[length(ss$order)+1]] <- name
  ss[["legendInfo"]][[name]] <- list(type="contour", levels=levels, col=col, lty=lty, lwd=lwd)
  putSS(ssm, ss)
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
#' @param low Low end of colour for background slice.
#' @param high High end of colour for background slice.
#' @param dimension Dimension to use for background slice. Will pick sensible
#'   default if NULL.
#' @param slice Slice number to use for background slice. Will pick sensible
#'   default if NULL.
#' @param col Colour of the background slice. Defaults to gray.
#' @param lineColour The colour of the slice indicator lines. Defaults to green.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' sliceSeries(nrow=5, begin=150, end=250) %>%
#'   anatomy(anatVol, low=700, high=1400) %>%
#'   overlay(stats, 2, 6, symmetric = T, alpha=0.1) %>%
#'   legend("t-statistics") %>%
#'   anatomySliceIndicator(anatVol, 700, 1400) %>%
#'   draw()
#' }
anatomySliceIndicator <- function(ssm, volume, low, high, dimension=NULL,
                                  slice=NULL, col=gray.colors(255, start=0),
                                  lineColour="green") {
  defDS <- sliceIndicatorDefaultDimensionAndSlice(ssm, volume)
  if (is.null(dimension)) dimension <- defDS[[1]]
  if (is.null(slice)) slice <- defDS[[2]]

  indVP <- setupSliceIndicatorVP(volume, dimension)
  indSlice <- sliceImage(volume, dimension,slice, low, high, col=col, vp=indVP)
  return(sliceIndicator(ssm, volume, dimension, indSlice, indVP))
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
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' sliceSeries(nrow=5, begin=150, end=250) %>%
#'   anatomy(anatVol, low=700, high=1400) %>%
#'   overlay(stats, 2, 6, symmetric = T, alpha=0.1) %>%
#'   legend("t-statistics") %>%
#'   contourSliceIndicator(anatVol, c(700, 1400)) %>%
#'   draw()
#' }
contourSliceIndicator <- function(ssm, volume, levels, dimension=NULL, slice=NULL, col="red", lty=1, lwd=1, lineColour="black") {
  defDS <- sliceIndicatorDefaultDimensionAndSlice(ssm, volume)
  if (is.null(dimension)) dimension <- defDS[[1]]
  if (is.null(slice)) slice <- defDS[[2]]


  indVP <- setupSliceIndicatorVP(volume, dimension)
  indContour <- sliceContours(volume, dimension,levels = levels, col=col, lty=lty, lwd=lwd,
                              slice=slice, vp=indVP)
  return(sliceIndicator(ssm, volume, dimension, indContour, indVP, "black"))
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
  if (dimension == ss$dimension) {
    stop("sliceIndicator dimension cannot be equal to sliceSeries dimension")
  }
  sliceDims <- dim(volume)[-dimension]
  overVP <- viewport(layout = grid.layout(1, 1, widths = sliceDims[1],
                                          heights = sliceDims[2],
                                          respect = T))
  #indSlice <- sliceImage(volume, dimension,slice, low, high, col=col, vp=indVP)
  lineList <- list()
  counter <- 1
  for (i in 1:ss$nrow) {
    for (j in 1:ss$ncol) {
      if ( (ss$dimension == 2 & dimension == 1) |
           (ss$dimension == 1) ) {
        xd <- c(rep(ss$slices[counter], 2))
        yd <- c(0,sliceDims[2])
      } else {
        xd <- c(0, sliceDims[1])
        yd <- c(rep(ss$slices[counter], 2))
      }
      lineList[[counter]] <- linesGrob(x = xd,
                                       y = yd,
                                       default.units = "native",
                                       vp=indVP, gp=gpar(col=lineColour))
      #message(ss$slices[counter])
      counter <- counter+1
    }
  }
  ss[["sliceIndicator"]] <- gTree(vp=overVP, children=do.call(gList, c(bgGrob, lineList)))
  putSS(ssm, ss)
  return(ssm)
}

assembleLegends <- function(ss, gp = gpar()) {

  # some weirdness for the layout; I want alternating legends and small spacers, so that's
  # what these next few lines do.
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
  for (i in 1:length(ss$legendOrder)) {
    li <- ss$legendInfo[[ss$legendOrder[[i]]]]
    if (li$type == "slice")
      gl <- gList(sliceLegendGrob(li$low, li$high, li$col,
                                  li$rCol, li$symmetric,
                                  colWidth = unit(1, "lines"),
                                  description = li$description, gp = gp))
    else if (li$type == "contour")
      gl <- gList(contourLegendGrob(levels=li$levels, col=li$col, lty=li$lty, lwd=li$lwd,
                                    description=li$description, gp = gp))

    legendGrobs[[i]] <- gTree(vp=viewport(layout.pos.row = (i*2)-1, x=1),
                              children=gl)
  }

  ll <- gTree(vp=legendViewport, children=do.call(gList, legendGrobs))

  # the legend widths depend on the number of digits in high and low. This causes misalignment. So set all to be the
  # max of the widths of the inner legend viewport
  # this should be doable through just parameters somewhere to a grob or viewport, but I could not quite make that work,
  # hence this workaround on overwriting widths
  #mwidth <- max(sapply(ll$children, function(x) x$children[[1]]$childrenvp$parent$layout$widths[2]))
  #for (i in 1:length(ll$children)) {
  #  ll$children[[i]]$children[[1]]$childrenvp$parent$layout$widths[2] <- unit(mwidth, "lines")
  #}


  if (is.null(ss$sliceIndicator)) {
    return(ll)
  } else {
    vpWithIndicator <- viewport(layout=grid.layout(nrow=2, ncol=1,
                                                   heights=unit(c(0.15,0.85), c("null", "null"))))
    ind <- gTree(vp=viewport(layout.pos.row = 1, layout.pos.col = 1),
                 children=gList(ss$sliceIndicator))
    rest <- gTree(vp=viewport(layout.pos.row = 2, layout.pos.col = 1),
                  children=gList(ll))
    return(
      gTree(vp=vpWithIndicator, children=gList(ind, rest))
    )
    #return(
    #  gTree(vp=viewport(layout = grid.layout()), children=gList(rest))
    #)

  }
}

grobifySliceSeries <- function(ss) {
  grobList <- list(rectGrob(gp=gpar(fill="black"))) # need to make the black rectangle optional or controlable

  for (i in 1:length(ss$order)) {
    grobList <- c(grobList, ss[[ss$order[[i]]]])
  }

  gT <- gTree(children=do.call(gList, grobList), vp=ss$seriesVP)
  #gT <- gTree(children=do.call(gList, grobList), vp=ss$seriesVP, childrenvp=ss$anatomy[[1]][[1]]$vp)
  #gT <- gTree(children=do.call(gList, grobList), childrenvp=ss$seriesVP)
  #gT <- gTree(do.call(gList, grobList), vp=ss$seriesVP)
  #gT <- gList(grobList, vp=ss$seriesVP)

  # if (length(ss$legendOrder) >0) {
  #   vO <- viewport(layout = grid.layout(1,2,
  #                                       widths=unit(c(1,4), c("null", "lines"))))
  #
  #   legend <- assembleLegends(ss)
  #
  #   g1 <- gTree(vp=viewport(layout.pos.col = 1), children=gList(gT))
  #   g2 <- gTree(vp=viewport(layout.pos.col = 2), children=gList(legend))
  #
  #   return(gTree(children=gList(g1, g2), vp=vO))
  # } else {
    return(gT)
  #}
}

#' Turn the slice series into grobs.
#'
#' Takes the slice series, creates grobs where necessary, and returns a gTree.
#' Can then be incorporated into other grid functions.
#'
#' @param ssm The slice series
#' @param layout Whether to lay out by column (the default) or by row.
#'
#' @return Retunrs a gTree
#' @export
#'
#' @examples
#' \dontrun{
#' sliceSeriesGridTree <- sliceSeries(nrow=5, begin=150, end=250) %>%
#'   anatomy(anatVol, low=700, high=1400) %>%
#'   overlay(stats, 2, 6, symmetric = T, alpha=0.1) %>%
#'   legend("t-statistics") %>%
#'   anatomySliceIndicator(anatVol, 700, 1400) %>%
#'   grobify()
#' }
grobify <- function(ssm, layout="column", titlePars = gpar(), legendPars = gpar(), bgCol = NULL) {
  if (layout=="column") grobifyByColumn(ssm, titlePars, legendPars, bgCol)
  else grobifyByRow(ssm, titlePars, legendPars, bgCol)
}

grobifyByRow <- function(ssm, titlePars = gpar(), legendPars = gpar(), bgCol = NULL) {
  nseries <- length(ssm$ssl)
  haveTitles <- any(sapply(ssm$ssl, function(x) length(x$title))>0)
  haveLegends <- any(sapply(ssm$ssl, function(x) length(x$legendOrder))>0)

  nrow <- nseries
  ncol <- nseries+haveTitles+haveLegends

  gs <- list()

  widths <- list()
  if (haveTitles) widths[[1]] <- unit(1, "lines")
  widths[[length(widths)+1]] <- unit(1, "null") #rep(unit(1, "null"), nseries))
  if (haveLegends) widths[[length(widths)+1]] <- unit(4, "lines")

  column <- haveTitles+1

  for (i in 1:nseries) {
    gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.row = i,
                                            layout.pos.col = column),
                                children=gList(grobifySliceSeries(ssm$ssl[[i]])))
    if (length(ssm$ssl[[i]]$title)>0) {
      gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.row = i,
                                              layout.pos.col = 1),
                                  children=gList(textGrob(ssm$ssl[[i]]$title, rot=90, gp = titlePars)))
    }
    if (length(ssm$ssl[[i]]$legendOrder)>0) {
      gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.col = 3, layout.pos.row = i),
                                  children=gList(assembleLegends(ssm$ssl[[i]], gp = legendPars)))
    }
  }

  if(!is.null(bgCol))
      gs <- c(list(rectGrob(gp = gpar(col = NA, fill = bgCol))), gs)

  vA <- viewport(layout = grid.layout(nrow, ncol,
                                      widths = do.call(unit.c, widths)))

  return(gTree(children=do.call(gList, gs), vp=vA))
}

grobifyByColumn <- function(ssm, titlePars = gpar(), legendPars = gpar(), bgCol = NULL) {
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
                     children=gList(grobifySliceSeries(ssm$ssl[[i]])))
    widths[[j]] <- unit(1, "null")
    j <- j+1
    # add the legend(s) if present
    if (length(ssm$ssl[[i]]$legendOrder) > 0) {
      gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.col = j, layout.pos.row = row),
                       children=gList(assembleLegends(ssm$ssl[[i]], gp = legendPars)))
      widths[[j]] <- unit(4, "lines")
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

#' Title
#'
#' @param ssm
#' @param layout
#'
#' @return
#' @export
#'
#' @examples
draw <- function(ssm, layout="column") {
  grid.newpage()
  l <- grobify(ssm, layout=layout)
  grid.draw(l)
}
