sliceSeries <- function(slices=NULL, nrow=4, ncol=5, dimension=2, begin=NULL, end=NULL) {
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
            legendOrder=list())
  class(l) <- c("sliceSeries", "list")
  return(l)
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

anatomy <- function(ss, volume, low, high, col=gray.colors(255, start=0), name="anatomy") {
  slice(ss, volume, low, high, col=col, name=name)
}

overlay <- function(ss, volume, low, high, col=mincDefaultCol(),
                    symmetric=FALSE, rCol=mincDefaultRCol(), underTransparent = TRUE, name="stats", box=FALSE) {

  slice(ss, volume, low, high, col=col, name=name, underTransparent = underTransparent, symmetric = symmetric,
        rCol=rCol, box=box)
}

legend <- function(ss, description=NULL) {
  ss$legendOrder <- c(ss$legendOrder, ss$order[[length(ss$order)]])
  ss[["legendInfo"]][[length(ss$order)]]$description = description
  return(ss)
}

slice <- function(ss, volume, low, high, col,reverse = FALSE, underTransparent = FALSE, symmetric=FALSE, rCol=mincDefaultRCol(),
                  name=NULL, box=FALSE) {
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
  return(ss)
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

grobify <- function(ss) {
  grobList <- list(rectGrob(gp=gpar(fill="black"))) # need to make the black rectangle optional or controlable

  for (i in 1:length(ss$order)) {
    grobList <- c(grobList, ss[[ss$order[[i]]]])
  }

  gT <- gTree(children=do.call(gList, grobList), vp=ss$seriesVP)

  if (length(ss$legendOrder) >0) {
    vO <- viewport(layout = grid.layout(1,2,
                                        widths=unit(c(1,4), c("null", "lines"))))

    legend <- assembleLegends(ss)

    g1 <- gTree(vp=viewport(layout.pos.col = 1), children=gList(gT))
    g2 <- gTree(vp=viewport(layout.pos.col = 2), children=gList(legend))

    return(gTree(children=gList(g1, g2), vp=vO))
  } else {
    return(gT)
  }
}

draw <- function(ss) {
  grid.newpage()
  l <- grobify(ss)
  grid.draw(l)
}
