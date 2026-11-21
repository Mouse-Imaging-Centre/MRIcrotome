test_that("maskBounds() gives padded slice bounds from a mask (#5)", {
  mask <- array(0L, c(20, 24, 28))
  mask[5:9, 10:15, 20:22] <- 1L
  expect_equal(maskBounds(mask, 1, padding = 2), c(begin = 3, end = 11))
  expect_equal(maskBounds(mask, 2, padding = 0), c(begin = 10, end = 15))
  expect_equal(maskBounds(mask, 3), c(begin = 10, end = 28))   # clamped to the volume
  expect_error(maskBounds(mask, 4), "dimension")
  expect_error(maskBounds(array(0L, c(2, 2, 2)), 1), "no non-zero")
  expect_error(maskBounds(matrix(1, 2, 2), 1), "3D array")
  b <- maskBounds(mask, 2)
  expect_no_error(sliceSeries(nrow = 2, ncol = 2, begin = b["begin"], end = b["end"]) %>%
                    anatomy(mask * 1000, low = 0, high = 1000))
})
