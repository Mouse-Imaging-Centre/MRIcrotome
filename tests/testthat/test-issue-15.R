test_that("legend(global = TRUE) spans all rows in row layout (#15)", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  threeViews <- function(global) {
    sliceSeries(nrow = 1, ncol = 3, begin = 5, end = 20, dimension = 2) %>%
      anatomy(v$anat, 0, 3000) %>% overlay(v$stats, 2, 6, symmetric = TRUE) %>%
      sliceSeries(dimension = 1) %>%
      anatomy(v$anat, 0, 3000) %>% overlay(v$stats, 2, 6, symmetric = TRUE) %>%
      sliceSeries(dimension = 3) %>%
      anatomy(v$anat, 0, 3000) %>% overlay(v$stats, 2, 6, symmetric = TRUE) %>%
      legend("t-statistics", global = global)
  }
  # the legend is the last grob added; its viewport gives the rows it covers
  legendRows <- function(g) tail(g$children, 1)[[1]]$vp$layout.pos.row
  expect_equal(legendRows(grobify(threeViews(FALSE), layout = "row")), c(3, 3))
  expect_equal(legendRows(grobify(threeViews(TRUE), layout = "row")), c(1, 3))
  expect_no_error(draw(threeViews(TRUE), layout = "row"))
  expect_no_error(draw(threeViews(TRUE)))
})
