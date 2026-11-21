#' Device size that fits a figure without padding
#'
#' Each slice series keeps the aspect ratio of its slices, so on a device of
#' another shape the slices are centred and the rest of their area shows the
#' slice background. \code{figureSize()} calculates the device width (or
#' height) that makes the slices fill their area, for the device height (or
#' width) that you give.
#'
#' @param ssm The slice series, as it would be given to \code{\link{draw}}.
#' @param width,height Give one of the two, in inches. The function returns
#'   the other one.
#' @param layout \code{"column"} (the default) or \code{"row"}, as for
#'   \code{\link{draw}}.
#' @param legendPars The \code{gpar} list that you give to \code{\link{draw}}
#'   or \code{\link{grobify}}; the width of a legend depends on its font
#'   size.
#'
#' @details The calculation follows the layout of \code{\link{grobify}} and
#'   the 5\% margin of \code{\link{draw}}. In column layout the series have
#'   equal widths, so when their slices have different aspect ratios the
#'   tallest series sets the height and the others have some padding. In row
#'   layout the series have equal heights, so the widest series sets the
#'   width. Legends are measured with the font size of \code{legendPars}.
#'
#' @return A named vector \code{c(width = , height = )} in inches, for
#'   example for \code{pdf()}, or multiplied by a resolution for \code{png()}.
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' fig <- sliceSeries(nrow = 2, ncol = 4, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics")
#' size <- figureSize(fig, width = 7)
#' size
#' f <- tempfile(fileext = ".pdf")
#' pdf(f, width = size["width"], height = size["height"])
#' draw(fig)
#' dev.off()
figureSize <- function(ssm, width = NULL, height = NULL, layout = "column",
                       legendPars = gpar()) {
  if (is.null(width) == is.null(height))
    stop("Give either 'width' or 'height', not both")
  given <- if (is.null(width)) height else width
  if (!is.numeric(given) || length(given) != 1 || is.na(given) || given <= 0)
    stop("'width' or 'height' must be one positive number")
  layout <- match.arg(layout, c("column", "row"))
  for (i in seq_along(ssm$ssl)) {
    if (is.null(ssm$ssl[[i]]$seriesVP))
      stop("Slice series ", i, " has no layers. Add anatomy(), overlay() or ",
           "contours() after sliceSeries().")
  }

  # height / width of the grid of slices of each series
  aspect <- vapply(ssm$ssl, function(ss) {
    lay <- ss$seriesVP$layout
    sum(as.numeric(lay$heights)) / sum(as.numeric(lay$widths))
  }, numeric(1))
  nseries <- length(ssm$ssl)
  margin <- 0.95   # draw() uses 95% of the device in each direction

  if (layout == "row") {
    sides <- vapply(ssm$ssl, function(x)
      if (length(x$title) > 0) x$titleSide %||% "right" else "", character(1))
    fraction <- 1 - 0.1 * (any(sides == "left") + any(sides == "right")) -
      0.2 * any(vapply(ssm$ssl, hasSidePanel, logical(1)))
    # the slice column is fraction * margin * width wide; each of the nseries
    # rows is margin * height / nseries high and must fit the widest series
    ratio <- nseries * fraction * max(aspect)   # height / width
    if (is.null(height)) return(c(width = width, height = width * ratio))
    return(c(width = height / ratio, height = height))
  }

  # column layout: legend columns and the title row have fixed sizes
  inches <- function(u, f) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    f(u, "inches", valueOnly = TRUE)
  }
  legendInches <- sum(vapply(ssm$ssl, function(ss)
    if (hasSidePanel(ss)) inches(legendWidth(ss, legendPars), convertWidth) else 0,
    numeric(1)))
  haveTitles <- any(vapply(ssm$ssl, function(x) length(x$title) > 0, logical(1)))
  titleInches <- if (haveTitles) inches(unit(1, "lines"), convertHeight) else 0
  # series height = series width * max(aspect); series width =
  # (margin * width - legends) / nseries; margin * height = series height + title
  a <- max(aspect) / nseries
  if (is.null(height)) {
    h <- (a * (margin * width - legendInches) + titleInches) / margin
    return(c(width = width, height = h))
  }
  w <- ((margin * height - titleInches) / a + legendInches) / margin
  c(width = w, height = height)
}
