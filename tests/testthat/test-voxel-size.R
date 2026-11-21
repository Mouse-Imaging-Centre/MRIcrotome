# voxels that are not square keep their proportions (#81)
cellSizes <- function(ss) {
  lay <- ss$seriesVP$layout
  c(as.numeric(lay$widths)[1], as.numeric(lay$heights)[1])
}

test_that("voxelSize scales the cells of a series", {
  v <- testVolumes()   # dimension 2 slices are 20 x 28 voxels
  ss <- (sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000))$ssl[[1]]
  expect_equal(cellSizes(ss), c(20, 28))
  ss <- (sliceSeries(nrow = 1, ncol = 2, voxelSize = c(1, 1, 2)) %>%
           anatomy(v$anat, 0, 3000))$ssl[[1]]
  expect_equal(cellSizes(ss), c(20, 56))
  # the raster keeps one pixel per voxel; the cell stretches it
  expect_equal(dim(as.matrix(ss$anatomy[[1]][[1]]$raster)), c(28, 20))
  # rotated: the separations turn with the slice
  ss <- (sliceSeries(nrow = 1, ncol = 2, voxelSize = c(1, 1, 2), rotate = 90) %>%
           anatomy(v$anat, 0, 3000))$ssl[[1]]
  expect_equal(cellSizes(ss), c(56, 20))
})

test_that("voxelSize carries over and reaches indicators and figureSize()", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  ssm <- sliceSeries(nrow = 1, ncol = 2, voxelSize = c(1, 1, 2)) %>% anatomy(v$anat, 0, 3000) %>%
    anatomySliceIndicator(v$anat, 0, 3000) %>%
    sliceSeries(dimension = 3) %>% anatomy()
  expect_equal(ssm$ssl[[2]]$voxelSize, c(1, 1, 2))
  expect_equal(cellSizes(ssm$ssl[[2]]), c(20, 24))    # dimension 3: axes 1 and 2
  ind <- ssm$ssl[[1]]$sliceIndicator$vp$layout        # indicator along dimension 1
  expect_equal(c(as.numeric(ind$widths), as.numeric(ind$heights)), c(24, 56))
  expect_no_error(draw(ssm))
  square <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000)
  tall <- sliceSeries(nrow = 1, ncol = 2, voxelSize = c(1, 1, 2)) %>% anatomy(v$anat, 0, 3000)
  expect_equal(figureSize(tall, width = 7, layout = "row")[["height"]],
               2 * figureSize(square, width = 7, layout = "row")[["height"]])
})

test_that("voxelSize is checked", {
  expect_error(sliceSeries(nrow = 1, voxelSize = c(1, 2)), "three positive numbers")
  expect_error(sliceSeries(nrow = 1, voxelSize = c(1, 0, 1)), "three positive numbers")
  expect_error(sliceSeries(nrow = 1, voxelSize = c("a", "b", "c")), "three positive numbers")
})
