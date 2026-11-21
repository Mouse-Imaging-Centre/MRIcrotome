# a volume with only NA values (#46 item 2.5)
test_that("an all-NA volume asks for low and high, and draws nothing with them", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  na <- array(NA_real_, dim(v$anat))
  base <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000)
  expect_error(base %>% overlay(na), "only NA values. Give 'low' and 'high'")
  expect_error(base %>% overlay(na, low = 2), "only NA values")
  # e.g. a thresholded statistics map in which no voxel survives
  ssm <- base %>% overlay(na, low = 2, high = 5) %>% legend("t")
  expect_no_error(draw(ssm))
  ss <- ssm$ssl[[1]]
  expect_true(all(is.na(as.vector(ss[[ss$order[[2]]]][[1]][[1]]$raster))))
})
