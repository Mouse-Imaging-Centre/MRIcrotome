# a negative end counts back from the last slice, as in RMINC (#80)
test_that("a negative end counts back from the last slice", {
  v <- testVolumes()   # 20 x 24 x 28
  ss <- (sliceSeries(nrow = 1, ncol = 3, begin = 5, end = -5) %>% anatomy(v$anat, 0, 3000))$ssl[[1]]
  expect_equal(ss$slices, c(5, 12, 19))   # dimension 2 has 24 slices: end 19
  expect_equal(ss$end, -5)
  # carried over as a negative value: the same margin along dimension 1
  two <- sliceSeries(nrow = 1, ncol = 3, begin = 5, end = -5) %>% anatomy(v$anat, 0, 3000) %>%
    sliceSeries(dimension = 1) %>% anatomy()
  expect_equal(two$ssl[[2]]$slices, c(5, 10, 15))   # 20 slices: end 15
  expect_error(sliceSeries(nrow = 1, ncol = 2, begin = 1, end = -30) %>% anatomy(v$anat),
               "Check 'begin' and 'end'")
})

test_that("begin = 11, end = -10 leaves out 10 slices at each side", {
  ss <- (sliceSeries(nrow = 1, ncol = 2, begin = 11, end = -10) %>%
           anatomy(exampleVolumes()$anatomy, 600, 1400))$ssl[[1]]
  expect_equal(ss$slices, c(11, 40))   # 50 slices: 1-10 and 41-50 are left out
})
