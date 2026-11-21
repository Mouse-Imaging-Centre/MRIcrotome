# anatomy() with no volume repeats the earlier call for the current series (#46 item 1.3)
test_that("anatomy() reuse follows the slices of the new series", {
  v <- testVolumes()
  ssm <- sliceSeries(nrow = 1, ncol = 3, begin = 5, end = 10) %>%
    anatomy(v$anat, 0, 3000) %>%
    sliceSeries(begin = 15, end = 20) %>%
    anatomy()
  s1 <- ssm$ssl[[1]]; s2 <- ssm$ssl[[2]]
  expect_equal(s2$slices, c(15, 18, 20))
  expect_false(identical(s1$anatomy, s2$anatomy))
  # the same grobs as an explicit call for the new series
  explicit <- sliceSeries(nrow = 1, ncol = 3, begin = 15, end = 20) %>% anatomy(v$anat, 0, 3000)
  rasters <- function(sl) lapply(sl, function(g) g[[1]]$raster)
  expect_equal(rasters(s2$anatomy), rasters(explicit$ssl[[1]]$anatomy))
  expect_equal(s2$legendInfo$anatomy, s1$legendInfo$anatomy)
})

test_that("anatomy() reuse follows the shape and dimension of the new series", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  base <- sliceSeries(nrow = 1, ncol = 3) %>% anatomy(v$anat, 0, 3000)
  wide <- base %>% sliceSeries(ncol = 5) %>% anatomy()
  expect_length(wide$ssl[[2]]$anatomy, 5)

  other <- base %>% sliceSeries(dimension = 1) %>% anatomy() %>% overlay(v$stats, 2, 5)
  expect_equal(other$ssl[[2]]$slices, c(1, 11, 20))
  expect_no_error(draw(other))

  # a slice indicator with no overlay needs the slices of the series
  ind <- base %>% sliceSeries(begin = 3, end = 9) %>% anatomy() %>% anatomySliceIndicator(v$anat, 0, 3000)
  expect_no_error(draw(ind))
})

test_that("anatomy() reuse skips series without anatomy, keeps the name, and takes overrides", {
  v <- testVolumes()
  ssm <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000, name = "bg") %>%
    sliceSeries() %>% overlay(v$stats, 2, 5, name = "ov") %>%
    sliceSeries() %>% anatomy(high = 1000)
  li <- ssm$ssl[[3]]$legendInfo$anatomy
  expect_equal(c(li$low, li$high), c(0, 1000))
  expect_error(sliceSeries(nrow = 1) %>% anatomy(), "first time")
})
