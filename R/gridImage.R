

sliceImage <- function(volume,
                       dimension = 2,
                       slice = NULL,
                       low = min(volume, na.rm = TRUE),
                       high = max(volume, na.rm = TRUE),
                       reverse = FALSE, underTransparent = FALSE,
                       col = gray.colors(255),
                       symmetric=FALSE,
                       rCol = mincDefaultRCol(),
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

mincGridPlotAnatAndStatsSlice <- function(anatomy,
                                          statistics,
                                          slice = NULL,
                                          dimension = 2,
                                          low = min(statistics,
                                                    na.rm = TRUE),
                                          high = max(statistics, na.rm = TRUE),
                                          anatLow = min(anatomy,
                                                        na.rm = TRUE),
                                          anatHigh = max(anatomy, na.rm = TRUE), symmetric = FALSE,
                                          col = NULL, rcol = NULL, legend = NULL) {

  if (length(dim(anatomy)) != 3)
    stop("anatomy must be 3 dimensional, you may be missing a call to mincArray")
  if (is.null(slice)) {
    halfdims <- ceiling(dim(anatomy)/2)
    slice <- halfdims[dimension]
  }
  if (is.null(col)) {
    if (symmetric == TRUE) {
      col <- colorRampPalette(c("red", "yellow"))(255)
    }
    else {
      col <- rainbow(255)
    }
  }
  if (is.null(rcol) && symmetric) {
    rcol <- colorRampPalette(c("blue", "turquoise1"))(255)
  }
  anatCols = gray.colors(255, start = 0)
  gA <- mincGridImage(anatomy, dimension, slice, col = anatCols,
            low = anatLow, high = anatHigh)
  gS1 <- mincGridImage(statistics, dimension, slice,
            col = col, underTransparent = TRUE, low = low, high = high)
  if (symmetric) {
    gS2 <- mincGridImage(statistics, dimension, slice,
              col = rcol, underTransparent = TRUE,
              reverse = TRUE, low = low, high = high)
    out <- gList(gA, gS1, gS2)
  }
  else { out <- gList(gA, gS1)}
  return(out)
}

mincDefaultCol <- function() {
  getOption("RMINCcol", colorRampPalette(c("red", "yellow"))(255))
}

mincDefaultRCol <- function() {
  getOption("RMINCrcol", colorRampPalette(c("blue", "turquoise1"))(255))
}

sliceLegendGrob <- function(low, high,
                       col = mincDefaultCol(),
                       rcol = mincDefaultRCol(),
                       symmetric=FALSE,
                       description=NULL,
                       lowText = NULL,
                       highText = NULL,
                       colWidth = unit(1, "null"),
                       gp=gpar()){

  # determine the maximum string width for purposes of setting viewport sizes
  if (symmetric==TRUE) {
    strings <- c(low, high, -low, -high)
  } else {
    strings <- c(low, high)
  }
  # format strings so that decimals align
  strings <- format(strings, drop0trailing = T)

  maxLength <- max(c(convertWidth(stringWidth(strings), "lines")))
  maxLength <- max(1, maxLength) # no smaller than 1 line

  # construct the viewports; with either have 2 or 3 elements depending on
  # whether there is an outer description
  if (is.null(description)) {
    ncol <- 2
    widths <- unit.c(colWidth, unit(maxLength, "lines"))
  } else {
    ncol <- 3
    widths <- unit.c(colWidth, unit(c(maxLength,1), c("lines", "lines")))
  }
  vLayout <- viewport(name="layout", w=0.4, #just="right",
                     layout=grid.layout(nrow=3, ncol=ncol,
                                        widths = widths,
                                        heights=unit(c(1,0.5,1), c("null", "lines", "null"))))

  # more viewport creation, with the number of viewports depending on whether the
  # legend is to be symmetric
  if (symmetric == TRUE) {
    vColH <- viewport(layout.pos.row = 1, layout.pos.col = 1, name="colsH")
    vColL <- viewport(layout.pos.row = 3, layout.pos.col = 1, name="colsL")
    vInnerH <- viewport(layout.pos.row = 1, layout.pos.col = 2, name="innerH")
    vInnerL <- viewport(layout.pos.row = 3, layout.pos.col = 2, name="innerL")
    vList <- list(vColH, vColL, vInnerH, vInnerL)
  } else {
    vColH <- viewport(layout.pos.row = 1:3, layout.pos.col = 1, name="colsH")
    vInnerH <- viewport(layout.pos.row = 1:3, layout.pos.col = 2, name="innerH")
    vList <- list(vColH, vInnerH)
  }

  # an extra viewport for the outer description if passed as an arg
  if (!is.null(description)) {
    vOuter <- viewport(layout.pos.row = 1:3, layout.pos.col = 3, name="outer")
    vList[[length(vList)+1]] <- vOuter
  }

  # assemble the list of viewports
  vT <- vpTree(vLayout, do.call(vpList, vList))

  # onto the list of grobs
  grobList <- list(
    rasterGrob(rev(col), vp=vpPath("layout", "colsH"),
               width = unit(1, "npc"), height=unit(1, "npc")),
    textGrob(strings[1], x=unit(0, "npc"), y = unit(0.05, "npc"),
             gp=gp, vp=vpPath("layout", "innerH"), just="left"),
    textGrob(strings[2], x=unit(0, "npc"),y = unit(0.95, "npc"),
             gp=gp, vp=vpPath("layout", "innerH"), just="left")
  )
  if (symmetric == TRUE) {
    grobList <- c(grobList, list(
      rasterGrob(rcol, vp=vpPath("layout", "colsL"),
                 width = unit(1, "npc"), height=unit(1, "npc")),
      textGrob(strings[3], x=unit(0, "npc"),y = unit(0.95, "npc"),
               gp=gp, vp=vpPath("layout", "innerL"), just="left"),
      textGrob(strings[4], x=unit(0, "npc"),y = unit(0.05, "npc"),
               gp=gp, vp=vpPath("layout", "innerL"), just="left")
    ))
  }

  #message("Length of grobsList:", length(grobList))
  if (!is.null(highText)) {
    grobList[[length(grobList)+1]] <-
                       textGrob(highText, y = unit(0.5, "npc"), rot=90, gp=gp,
                                vp=vpPath("layout", "innerH"))
  }
  if (!is.null(lowText) & symmetric==TRUE) {
    grobList[[length(grobList)+1]] <-
                       textGrob(lowText, y = unit(0.5, "npc"), rot=90, gp=gp,
                                vp=vpPath("layout", "innerL"))
  }
  if (!is.null(description)) {
    grobList[[length(grobList)+1]] <-
      textGrob(description, y = unit(0.5, "npc"), rot=90, gp=gp,
               vp=vpPath("layout", "outer"))
  }
  gl <- do.call(gList, grobList)
  gTree(name="legend", children = gl, childrenvp = vT, cl = "mincLegend")
}

# the display width for calculating minimum size
widthDetails.mincLegend <- function(x){
  # sum up the sizes; but since the actual colours are "null", add 1 line
  sum(layout.widths(viewport.layout(x$childrenvp[[1]])))+unit(1, "lines")
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
