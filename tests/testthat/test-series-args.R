# sliceSeries() refuses arguments that cannot work, with a clear message (#46 item 2.2)
test_that("dimension must be 1, 2 or 3", {
  v <- testVolumes()
  expect_error(sliceSeries(nrow = 1, dimension = 4), "must be 1, 2, or 3")
  expect_error(sliceSeries(nrow = 1, dimension = 0), "must be 1, 2, or 3")
  expect_error(sliceSeries(nrow = 1, dimension = "x"), "must be 1, 2, or 3")
  base <- sliceSeries(nrow = 1) %>% anatomy(v$anat, 0, 3000)
  expect_error(base %>% sliceSeries(dimension = 4), "must be 1, 2, or 3")
  for (d in 1:3) expect_no_error(sliceSeries(nrow = 1, dimension = d) %>% anatomy(v$anat, 0, 3000))
})

test_that("nrow and ncol must be whole numbers of 1 or more", {
  expect_error(sliceSeries(nrow = 0), "'nrow' must be one whole number")
  expect_error(sliceSeries(nrow = 1, ncol = 2.5), "'ncol' must be one whole number")
  expect_error(sliceSeries(nrow = c(1, 2)), "'nrow' must be one whole number")
})

# the missing nrow or ncol comes from the number of slices (#46 item 2.3)
test_that("nrow or ncol is calculated from slices", {
  v <- testVolumes()
  grid <- function(ssm) c(ssm$ssl[[ssm$seriesCounter]]$nrow, ssm$ssl[[ssm$seriesCounter]]$ncol)
  expect_equal(grid(sliceSeries(slices = c(5, 10, 15))), c(1, 3))
  expect_equal(grid(sliceSeries(nrow = 1, slices = c(5, 10, 15))), c(1, 3))
  expect_equal(grid(sliceSeries(ncol = 2, slices = c(2, 4, 6, 8))), c(2, 2))
  expect_equal(grid(sliceSeries(nrow = 2, ncol = 2, slices = 1:4)), c(2, 2))
  expect_no_warning(sliceSeries(nrow = 1, slices = c(5, 10, 15)) %>% anatomy(v$anat, 0, 3000))
  # a later series keeps the rows of the series before it
  two <- sliceSeries(nrow = 2, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>% sliceSeries(slices = 1:6)
  expect_equal(grid(two), c(2, 3))
  expect_error(sliceSeries(), "Must specify either nrow or ncol")
})
