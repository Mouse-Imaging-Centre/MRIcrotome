# slices outside the volume give a clear message (#46 item 2.1)
test_that("slice numbers must be inside the volume", {
  v <- testVolumes()   # 20 x 24 x 28; dimension 2 has 24 slices
  expect_error(sliceSeries(nrow = 1, ncol = 3, begin = 0, end = 10) %>% anatomy(v$anat),
               "from 1 to 24 .*Not valid: 0. Check 'begin' and 'end'")
  expect_error(sliceSeries(nrow = 1, ncol = 3, begin = 1, end = 99) %>% anatomy(v$anat),
               "Not valid: 50, 99. Check 'begin' and 'end'")
  expect_error(sliceSeries(ncol = 2, slices = c(5, 50)) %>% anatomy(v$anat),
               "Not valid: 50\\.$")
  expect_error(sliceSeries(ncol = 2, slices = c(5, 2.5)) %>% overlay(v$stats, 2, 5), "Not valid: 2.5")
  expect_error(sliceSeries(ncol = 2, slices = c(5, 30), dimension = 1) %>%
                 contours(v$anat, levels = 1500), "from 1 to 20")
  expect_no_error(sliceSeries(ncol = 2, slices = c(1, 24)) %>% anatomy(v$anat))
})
