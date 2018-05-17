initMetaSliceSeries <- function() {
  ssm <- new.env(parent = globalenv())
  ssm$seriesCounter=1
  ssm$ssl=list()
  return(ssm)
}

#' Title
#'
#' @param ssm
#' @param slices
#' @param nrow
#' @param ncol
#' @param dimension
#' @param begin
#' @param end
#'
#' @return
#' @export
#'
#' @examples
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
            begin=if (is.null(begin) & ssm$seriesCounter>1) {ssm$ssl[[ssm$seriesCounter-1]]$begin} else {begin },
            end=if (is.null(end) & ssm$seriesCounter>1) {ssm$ssl[[ssm$seriesCounter-1]]$end} else {end},
            seriesVP=NULL, #seriesVP,
            order=list(),
            legendInfo=list(),
            legendOrder=list(),
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

#' Title
#'
#' @param ssm
#' @param volume
#' @param low
#' @param high
#' @param col
#' @param name
#'
#' @return
#' @export
#'
#' @examples
anatomy <- function(ssm, volume=NULL, low=NULL, high=NULL, col=gray.colors(255, start=0), name="anatomy") {
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
    slice(ssm, volume, low, high, col=col, name=name)
  }
}

#' Title
#'
#' @param ssm
#' @param volume
#' @param low
#' @param high
#' @param col
#' @param symmetric
#' @param rCol
#' @param underTransparent
#' @param name
#' @param box
#'
#' @return
#' @export
#'
#' @examples
overlay <- function(ssm, volume, low, high, col=mincDefaultCol(),
                    symmetric=FALSE, rCol=mincDefaultRCol(),
                    underTransparent = TRUE, name="stats", box=FALSE) {

  slice(ssm, volume, low, high, col=col, name=name, underTransparent = underTransparent, symmetric = symmetric,
        rCol=rCol, box=box)
}



#' Title
#'
#' @param ssm
#' @param title
#'
#' @return
#' @export
#'
#' @examples
addtitle <- function(ssm, title) {
  ss <- getSS(ssm)
  ss$title <- title
  putSS(ssm, ss)
  return(ssm)
}

#' Title
#'
#' @param ssm
#' @param description
#'
#' @return
#' @export
#'
#' @examples
legend <- function(ssm, description=NULL) {
  ss <- getSS(ssm)
  ss$legendOrder <- c(ss$legendOrder, ss$order[[length(ss$order)]])
  ss[["legendInfo"]][[length(ss$order)]]$description = description
  putSS(ssm, ss)
  return(ssm)
}

slice <- function(ssm, volume, low, high, col,reverse = FALSE, underTransparent = FALSE, symmetric=FALSE, rCol=mincDefaultRCol(),
                  name=NULL, box=FALSE) {
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
                                         rCol = rCol, box=box,
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

#' Title
#'
#' @param ssm
#' @param volume
#' @param levels
#' @param col
#' @param lty
#' @param lwd
#' @param name
#'
#' @return
#' @export
#'
#' @examples
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

assembleLegends <- function(ss) {

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
                                  description = li$description))
    else if (li$type == "contour")
      gl <- gList(contourLegendGrob(levels=li$levels, col=li$col, lty=li$lty, lwd=li$lwd, description=li$description))

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
  return(ll)
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

grobify <- function(ssm, layout="column") {
  if (layout=="column") grobifyByColumn(ssm)
  else grobifyByRow(ssm)
}

grobifyByRow <- function(ssm) {
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
                                  children=gList(textGrob(ssm$ssl[[i]]$title, rot=90)))
    }
    if (length(ssm$ssl[[i]]$legendOrder)>0) {
      gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.col = 3, layout.pos.row = i),
                                  children=gList(assembleLegends(ssm$ssl[[i]])))
    }
  }

  vA <- viewport(layout = grid.layout(nrow, ncol,
                                      widths = do.call(unit.c, widths)))

  return(gTree(children=do.call(gList, gs), vp=vA))

}

grobifyByColumn <- function(ssm) {
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
                                  children=gList(textGrob(ssm$ssl[[i]]$title)))
    }
    # add the slices
    gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.col = j, layout.pos.row = row),
                     children=gList(grobifySliceSeries(ssm$ssl[[i]])))
    widths[[j]] <- unit(1, "null")
    j <- j+1
    # add the legend(s) if present
    if (length(ssm$ssl[[i]]$legendOrder) > 0) {
      gs[[length(gs)+1]] <- gTree(vp=viewport(layout.pos.col = j, layout.pos.row = row),
                       children=gList(assembleLegends(ssm$ssl[[i]])))
      widths[[j]] <- unit(4, "lines")
      j <- j+1
    }
  }
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
