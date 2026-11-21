# Visual regression tests: each figure is drawn to an SVG and compared with
# the snapshot in _snaps/visual/ (see helper-visual.R). When a change moves
# pixels on purpose, check the new figures with testthat::snapshot_review()
# and accept them.

v <- testVolumes()

test_that("column layout: anatomy, symmetric overlay and legend", {
  expect_figure("column-overlay-legend", function() {
    sliceSeries(nrow = 2, ncol = 3, begin = 4, end = 20) %>%
      anatomy(v$anat, low = 0, high = 3000) %>%
      overlay(v$stats, low = 2, high = 5, symmetric = TRUE) %>%
      legend("t-statistics") %>%
      draw()
  })
})

test_that("row layout: two series with titles", {
  expect_figure("row-titles", function() {
    sliceSeries(nrow = 1, ncol = 4, begin = 4, end = 20) %>%
      anatomy(v$anat, low = 0, high = 3000) %>%
      addtitle("Anatomy") %>%
      sliceSeries() %>%
      anatomy() %>%
      overlay(v$stats, low = 2, high = 5) %>%
      addtitle("Stats", side = "left") %>%
      legend("t") %>%
      draw(layout = "row")
  })
})

test_that("contours by level and by label, with legends", {
  expect_figure("contours", function() {
    sliceSeries(nrow = 1, ncol = 3, begin = 6, end = 16) %>%
      anatomy(v$anat, low = 0, high = 3000) %>%
      contours(v$anat, levels = c(1500, 2500), col = c("green", "red"), name = "c1") %>%
      legend("intensity") %>%
      contours(v$labels, labels = 2, col = "magenta", name = "c2") %>%
      draw()
  })
})

test_that("slice indicators", {
  expect_figure("anatomy-slice-indicator", function() {
    sliceSeries(nrow = 1, ncol = 3, begin = 6, end = 18) %>%
      anatomy(v$anat, low = 0, high = 3000) %>%
      overlay(v$stats, low = 2, high = 5) %>%
      anatomySliceIndicator(v$anat, 0, 3000) %>%
      draw()
  })
  expect_figure("contour-slice-indicator", function() {
    sliceSeries(nrow = 1, ncol = 3, begin = 6, end = 18, dimension = 3) %>%
      anatomy(v$anat, low = 0, high = 3000) %>%
      contourSliceIndicator(v$anat, levels = 1500, lineColour = "white") %>%
      draw()
  })
})

test_that("rotated and mirrored series", {
  expect_figure("rotate-flip", function() {
    sliceSeries(nrow = 1, ncol = 3, begin = 6, end = 18) %>%
      anatomy(v$anat, low = 0, high = 3000) %>%
      contours(v$labels, labels = 2, col = "magenta") %>%
      addtitle("none") %>%
      sliceSeries(rotate = 90) %>%
      anatomy() %>%
      contours(v$labels, labels = 2, col = "magenta") %>%
      addtitle("rotate = 90") %>%
      sliceSeries(rotate = 0, flip = "horizontal") %>%
      anatomy() %>%
      contours(v$labels, labels = 2, col = "magenta") %>%
      addtitle("flip") %>%
      draw(layout = "row")
  })
})

test_that("voxels that are not square", {
  expect_figure("voxel-size", function() {
    sliceSeries(nrow = 1, ncol = 3, begin = 6, end = 18, voxelSize = c(1, 1, 2)) %>%
      anatomy(v$anat, low = 0, high = 3000) %>%
      contours(v$labels, labels = 2, col = "magenta") %>%
      anatomySliceIndicator(v$anat, 0, 3000) %>%
      draw()
  })
})
