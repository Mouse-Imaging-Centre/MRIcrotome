initMetaSliceSeries <- function() {
  ssm <- new.env(parent = globalenv())
  ssm$seriesCounter=1
  ssm$ssl=list()
  return(ssm)
}

sliceSeries <- function(ssm=NULL,
                        slices=NULL, nrow=4, ncol=5, dimension=2, begin=NULL, end=NULL) {

  # ssm (the first arg) is provided if this is part of an ongoing pipe, but should be
  # null if this is the first initialization
  if (is.null(ssm)) {
    ssm <- initMetaSliceSeries()
  }
  else {
    ssm$seriesCounter = ssm$seriesCounter+1
  }
  # add checking for slices vs nrow and ncol
  gl <- grid.layout(nrow=nrow, ncol=ncol)
  seriesVP <- viewport(layout = gl)
  l <- list(nrow=nrow,
            ncol=ncol,
            dimension=dimension,
            slices=slices,
            begin=begin,
            end=end,
            seriesVP=seriesVP,
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
  if (is.null(ss$slices)) {
    d <- dim(volume)
    if (is.null(ss$begin)) ss$begin <- 1
    if (is.null(ss$end)) ss$end <- d[ss$dimension]
    nslices <- ss$nrow*ss$ncol
    ss$slices <- ceiling(seq(ss$begin, ss$end, length=nslices))
  }

  return(ss)
}

anatomy <- function(ssm, volume, low, high, col=gray.colors(255, start=0), name="anatomy") {
  slice(ssm, volume, low, high, col=col, name=name)
}

overlay <- function(ssm, volume, low, high, col=mincDefaultCol(),
                    symmetric=FALSE, rCol=mincDefaultRCol(),
                    underTransparent = TRUE, name="stats", box=FALSE) {

  slice(ssm, volume, low, high, col=col, name=name, underTransparent = underTransparent, symmetric = symmetric,
        rCol=rCol, box=box)
}

addtitle <- function(ssm, title) {
  ss <- getSS(ssm)
  ss$title <- title
  putSS(ssm, ss)
  return(ssm)
}

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
  for (i in 1:ss$nrow) {
    for (j in 1:ss$ncol) {
      sliceList[[counter]] <- sliceImage(volume, ss$dimension,low,high,col=col,
                                         slice=ss$slices[counter],
                                         underTransparent = underTransparent,
                                         symmetric = symmetric,
                                         rCol = rCol, box=box,
                                         vp=viewport(layout.pos.row = i,
                                                     layout.pos.col = j))
      counter <- counter+1
    }
  }
  ss[[name]] <- sliceList
  ss[["legendInfo"]][[name]] <- list(low=low, high=high, col=col, rCol=rCol, symmetric=symmetric)
  ss[["order"]][[length(ss$order)+1]] <- name
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
    legendGrobs[[i]] <- gTree(vp=viewport(layout.pos.row = (i*2)-1, x=1),
                              children=gList(sliceLegendGrob(li$low, li$high, li$col,
                                                             li$rCol, li$symmetric,
                                                             colWidth = unit(1, "lines"),
                                                             description = li$description)))

  }

  ll <- gTree(vp=legendViewport, children=do.call(gList, legendGrobs))

  # the legend widths depend on the number of digits in high and low. This causes misalignment. So set all to be the
  # max of the widths of the inner legend viewport
  # this should be doable through just parameters somewhere to a grob or viewport, but I could not quite make that work,
  # hence this workaround on overwriting widths
  mwidth <- max(sapply(ll$children, function(x) x$children[[1]]$childrenvp$parent$layout$widths[2]))
  for (i in 1:length(ll$children)) {
    ll$children[[i]]$children[[1]]$childrenvp$parent$layout$widths[2] <- unit(mwidth, "lines")
  }
  return(ll)
}

grobifySliceSeries <- function(ss) {
  grobList <- list(rectGrob(gp=gpar(fill="black"))) # need to make the black rectangle optional or controlable

  for (i in 1:length(ss$order)) {
    grobList <- c(grobList, ss[[ss$order[[i]]]])
  }

  gT <- gTree(children=do.call(gList, grobList), vp=ss$seriesVP)

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

grobify <- function(ssm) {
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

draw <- function(ssm) {
  grid.newpage()
  l <- grobify(ssm)
  grid.draw(l)
}
