rasterGrobs <- function(g) {
  out <- list()
  walk <- function(x) {
    if (inherits(x, "rastergrob")) out[[length(out) + 1]] <<- x
    if (inherits(x, "gTree")) for (ch in x$children) walk(ch)
  }
  walk(g)
  out
}

test_that("overlay(interpolate = FALSE) reaches the raster grobs (#18)", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  g <- sliceSeries(nrow = 1, ncol = 2, begin = 5, end = 20) |>
    anatomy(v$anat, low = 0, high = 3000) |>
    overlay(v$labels, low = 1, high = 2, col = c("red", "blue"), interpolate = FALSE) |>
    grobify()
  interp <- sapply(rasterGrobs(g), function(r) r$interpolate)
  expect_equal(interp, c(TRUE, TRUE, FALSE, FALSE))
})
