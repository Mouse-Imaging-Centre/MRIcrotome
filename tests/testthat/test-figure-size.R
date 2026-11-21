# figureSize() gives a device size that fits the slices (from issue #10)
test_that("figureSize() is consistent in both directions and both layouts", {
  v <- testVolumes()
  fig <- sliceSeries(nrow = 2, ncol = 3, begin = 4, end = 20) %>%
    anatomy(v$anat, 0, 3000) %>% overlay(v$stats, 2, 5) %>% legend("t")
  s <- figureSize(fig, width = 7)
  expect_named(s, c("width", "height"))
  expect_equal(unname(figureSize(fig, height = s[["height"]])["width"]), 7)
  # wider devices are taller; the legend has a fixed width, so not in proportion
  expect_gt(figureSize(fig, width = 14)[["height"]], 2 * s[["height"]])

  row <- fig %>% sliceSeries() %>% anatomy() %>% addtitle("B")
  r <- figureSize(row, width = 7, layout = "row")
  expect_equal(figureSize(row, width = 14, layout = "row")[["height"]], 2 * r[["height"]])
  expect_equal(unname(figureSize(row, height = r[["height"]], layout = "row")["width"]), 7)
  # two series of 2 x 3 slices of 20 x 28 voxels, each row 60% of the width
  expect_equal(r[["height"]], 7 * 2 * 0.7 * (2 * 28) / (3 * 20))
})

test_that("figureSize() checks its arguments", {
  v <- testVolumes()
  fig <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000)
  expect_error(figureSize(fig), "either 'width' or 'height'")
  expect_error(figureSize(fig, width = 7, height = 5), "either 'width' or 'height'")
  expect_error(figureSize(fig, width = 0), "positive")
  expect_error(figureSize(fig, width = 7, layout = "grid"), "should be one of")
  expect_error(figureSize(sliceSeries(nrow = 1), width = 7), "has no layers")
})

test_that("figureSize() leaves the current graphics device as it was", {
  v <- testVolumes()
  fig <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>% legend("a")
  pdf(NULL); first <- dev.cur()
  pdf(NULL); pdf(NULL)
  on.exit(while (dev.cur() > 1 && dev.cur() >= first) dev.off(), add = TRUE)
  dev.set(first + 1)
  before <- dev.cur()
  n <- length(dev.list())
  figureSize(fig, width = 7)
  expect_equal(dev.cur(), before)
  expect_length(dev.list(), n)
})

test_that("figureSize() refuses the same figures that draw() refuses", {
  v <- testVolumes()
  # the second series has only a slice indicator, so draw() stops
  f <- sliceSeries(nrow = 1, ncol = 3) %>% anatomy(v$anat, 0, 3000) %>%
    sliceSeries(dimension = 1) %>% anatomySliceIndicator(v$anat, 0, 3000, dimension = 2)
  expect_error(grobify(f), "Slice series 2 has no layers")
  expect_error(figureSize(f, width = 7), "Slice series 2 has no layers")
})
