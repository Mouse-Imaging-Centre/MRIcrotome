# the labels shown next to a colour bar, formatted together so decimals align
legendStrings <- function(low, high, symmetric = FALSE) {
  strings <- if (isTRUE(symmetric)) c(low, high, -low, -high) else c(low, high)
  format(strings, drop0trailing = TRUE)
}

# width needed by the widest legend of a slice series: the sample column, the
# labels, the optional description, and a one-line margin; never below 4 lines
legendWidth <- function(ss, gp = gpar()) {
  w <- unit(4, "lines")
  for (n in ss$legendOrder) {
    li <- ss$legendInfo[[n]]
    strings <- if (li$type == "slice") legendStrings(li$low, li$high, li$symmetric) else format(li$levels)
    w <- max(w, grobWidth(textGrob(strings, gp = gp)) + unit(2 + !is.null(li$description), "lines"))
  }
  w
}

contourLegendGrob <- function(levels,
                              col,
                              lty,
                              lwd,
                              description=NULL,
                              colWidth=unit(1, "null"),
                              gp = gpar()
                              ) {

  nrows <- length(levels)

  maxWidth <- grobWidth(textGrob(levels, gp = gp))

  if (is.null(description)) {
    ncols <- 2
    widths <- unit.c(colWidth, maxWidth)
  } else {
    ncols <- 3
    widths <- unit.c(colWidth, maxWidth, unit(1, "lines"))
  }

  vLayout <- viewport(name="layout",
                      layout=grid.layout(nrow = nrows, ncol = ncols,
                                         widths = widths))

  col <- col[floor(seq.int(from=1, to=length(col), length.out = length(levels)))]
  lty <- lty[floor(seq.int(from=1, to=length(lty), length.out = length(levels)))]
  lwd <- lwd[floor(seq.int(from=1, to=length(lwd), length.out = length(levels)))]

  lgrobs <- pmap( list(row=1:length(levels), col=col, lty=lty, lwd=lwd), function(row,col,lty,lwd)
    linesGrob(x=unit(c(0,1), "native"),
              y=unit(c(0.5,0.5), "native"),
              gp=gpar(col=col, lwd=lwd, lty=lty), vp=viewport(layout.pos.col = 1,layout.pos.row = row)) )
  tgrobs <- map2(levels, 1:length(levels), ~ textGrob(.x, vp=viewport(layout.pos.row = .y,
                                                                      layout.pos.col = 2),
                                                      gp = gp))

  if (!is.null(description)) {
    tgrobs <- c(tgrobs,
                list(textGrob(description, y = unit(0.5, "npc"), rot=90, vp=viewport(layout.pos.row=1:length(levels),
                                                                                layout.pos.col=3), gp = gp)))
  }

  gTree(name="contourLegend", children=do.call(gList, c(lgrobs, tgrobs)), vp = vLayout)


}

sliceLegendGrob <- function(low, high,
                            col = defaultCol(),
                            rcol = defaultRCol(),
                            symmetric=FALSE,
                            description=NULL,
                            lowText = NULL,
                            highText = NULL,
                            colWidth = unit(1, "null"),
                            gp=gpar()){

  strings <- legendStrings(low, high, symmetric)
  # widest label, measured at draw time with the legend's own gp so that it is
  # right on any device and font size (measuring here would use whatever device
  # and gpar happen to be current while the pipe is being built)
  maxLength <- max(unit(1, "lines"), grobWidth(textGrob(strings, gp = gp)))

  # construct the viewports; with either have 2 or 3 elements depending on
  # whether there is an outer description
  if (is.null(description)) {
    ncol <- 2
    widths <- unit.c(colWidth, maxLength)
  } else {
    ncol <- 3
    widths <- unit.c(colWidth, maxLength, unit(1, "lines"))
  }
  vLayout <- viewport(name="layout", x=0.55*sum(widths),
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
    textGrob(strings[1], x=unit(0, "npc"), y = unit(0.0, "npc"),
             gp=gp, vp=vpPath("layout", "innerH"), just=c("left", "bottom")),
    textGrob(strings[2], x=unit(0, "npc"),y = unit(1, "npc"),
             gp=gp, vp=vpPath("layout", "innerH"), just=c("left", "top"))
  )
  if (symmetric == TRUE) {
    grobList <- c(grobList, list(
      rasterGrob(rcol, vp=vpPath("layout", "colsL"),
                 width = unit(1, "npc"), height=unit(1, "npc")),
      textGrob(strings[3], x=unit(0, "npc"),y = unit(1, "npc"),
               gp=gp, vp=vpPath("layout", "innerL"), just=c("left", "top")),
      textGrob(strings[4], x=unit(0, "npc"),y = unit(0.0, "npc"),
               gp=gp, vp=vpPath("layout", "innerL"), just=c("left", "bottom"))
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
  gTree(name="legend", children = gl, childrenvp = vT)
}
