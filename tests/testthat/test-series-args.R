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
