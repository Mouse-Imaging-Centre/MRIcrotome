# a constant volume, or low == high, is drawn and gives a warning (#46 item 2.6)
test_that("equal low and high draw the first colour with a warning", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  cst <- array(5, dim(v$anat))
  expect_warning(ssm <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(cst), "only one value")
  r <- as.vector(ssm$ssl[[1]]$anatomy[[1]][[1]]$raster)
  expect_true(all(r == gray.colors(255, start = 0)[1]))
  expect_no_error(draw(ssm))

  # a label value as both ends: the voxels of that label get the first colour
  expect_warning(ov <- sliceSeries(nrow = 1, ncol = 1, slices = 8) %>%
                   overlay(v$labels, low = 1, high = 1, col = "red"), "only one value")
  r <- as.matrix(ov$ssl[[1]][[ov$ssl[[1]]$order[[1]]]][[1]][[1]]$raster)
  expect_equal(sort(unique(as.vector(r)), na.last = TRUE), c("red", NA))
})

test_that("a symmetric overlay with low == high colours only voxels beyond the value", {
  v <- testVolumes()
  expect_warning(ssm <- sliceSeries(nrow = 1, ncol = 1, slices = 12) %>%
                   overlay(v$stats, 3, 3, symmetric = TRUE, col = "red", rCol = "blue"),
                 "only one value")
  g <- ssm$ssl[[1]][[ssm$ssl[[1]]$order[[1]]]][[1]]
  slice12 <- v$stats[, 12, ]
  pos <- as.vector(as.matrix(g[[1]]$raster)); neg <- as.vector(as.matrix(g[[2]]$raster))
  expect_equal(sum(!is.na(pos)), sum(slice12 >= 3))
  expect_equal(sum(!is.na(neg)), sum(slice12 <= -3))
})
