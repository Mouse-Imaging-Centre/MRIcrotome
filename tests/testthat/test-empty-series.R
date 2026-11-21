# clear messages for a series with no layers (#46 item 2.4)
test_that("an empty series and legend() with no layer give clear messages", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  expect_error(sliceSeries(nrow = 1) %>% draw(), "Slice series 1 has no layers")
  expect_error(sliceSeries(nrow = 1) %>% anatomy(v$anat) %>% sliceSeries() %>% grobify(),
               "Slice series 2 has no layers")
  expect_error(sliceSeries(nrow = 1) %>% legend("x"), "must come after a layer")
})

test_that("a slice indicator can come before the first layer", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  ssm <- sliceSeries(nrow = 1, ncol = 2) %>% anatomySliceIndicator(v$anat, 0, 3000) %>%
    anatomy(v$anat, 0, 3000)
  expect_equal(ssm$ssl[[1]]$slices, c(1, 24))
  expect_no_error(draw(ssm))
  expect_no_error(sliceSeries(nrow = 1, ncol = 2) %>% contourSliceIndicator(v$anat, 1500) %>%
                    anatomy(v$anat, 0, 3000) %>% draw())
})
