# number of slice-indicator lines (drawn in lineColour) anywhere in a grob tree
indicatorLines <- function(g, colour = "green") {
  n <- 0
  walk <- function(x) {
    if (inherits(x, "lines") && identical(x$gp$col, colour)) n <<- n + 1
    if (inherits(x, "gTree")) for (ch in x$children) walk(ch)
  }
  walk(g)
  n
}

test_that("slice indicators are drawn with or without a legend (#3)", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  base <- function() sliceSeries(nrow = 2, ncol = 2, begin = 5, end = 20) |> anatomy(v$anat, 0, 3000)
  expect_equal(indicatorLines(base() |> grobify()), 0)
  for (layout in c("column", "row")) {
    noLegend <- base() |> anatomySliceIndicator(v$anat, 0, 3000) |> grobify(layout = layout)
    withLegend <- base() |> legend("anatomy") |> anatomySliceIndicator(v$anat, 0, 3000) |> grobify(layout = layout)
    expect_equal(indicatorLines(noLegend), 4)
    expect_equal(indicatorLines(withLegend), 4)   # not drawn twice
    expect_no_error(grid.draw(noLegend))
  }
  expect_equal(indicatorLines(base() |> contourSliceIndicator(v$anat, 1500, lineColour = "green") |> grobify()), 4)
})
