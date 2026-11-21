# contours and indicator lines sit on the voxels of the image (#46 items 1.5, 1.6)
contourRange <- function(g, axis) range(unlist(lapply(g, function(l) as.numeric(l[[axis]]))))

# native rows covered by the non-background pixels of a raster grob
rasterRows <- function(r) {
  m <- as.matrix(r$raster)
  rowsFromTop <- range(which(apply(m != m[1, 1], 1, any)))
  sort(nrow(m) - rowsFromTop + c(1, 0))   # native y of the bottom and top edges
}

test_that("a label contour follows the voxel edges", {
  v <- testVolumes()   # label 1 fills voxels 6:10 on each axis; slices are 20 x 24
  g <- sliceContours(v$labels, 3, 8, levels = 0.5)
  expect_equal(contourRange(g, "x"), c(5, 10))
  expect_equal(contourRange(g, "y"), c(5, 10))
  expect_equal(rasterRows(sliceImage(v$labels, 3, 8, 0, 2)[[1]]), c(5, 10))
})

test_that("with RMINC_flip_image = FALSE, contours flip with the image", {
  v <- testVolumes()
  op <- options(RMINC_flip_image = FALSE); on.exit(options(op))
  g <- sliceContours(v$labels, 3, 8, levels = 0.5)
  expect_equal(contourRange(g, "x"), c(5, 10))
  expect_equal(contourRange(g, "y"), c(14, 19))
  expect_equal(rasterRows(sliceImage(v$labels, 3, 8, 0, 2)[[1]]), c(14, 19))
})

test_that("slice indicator lines go through the centre of the slice", {
  v <- testVolumes()
  lineAt <- function(ssm, axis) {
    ind <- ssm$ssl[[1]]$sliceIndicator
    unname(vapply(utils::tail(as.list(ind$children), 2), function(l) as.numeric(l[[axis]])[1], numeric(1)))
  }
  # series along dimension 2, indicator from dimension 1: vertical lines (x)
  s2 <- sliceSeries(nrow = 1, ncol = 2, slices = c(3, 10)) %>% anatomy(v$anat, 0, 3000)
  expect_equal(lineAt(s2 %>% anatomySliceIndicator(v$anat, 0, 3000), "x"), c(2.5, 9.5))
  # series along dimension 3, indicator from dimension 1: horizontal lines (y)
  s3 <- sliceSeries(nrow = 1, ncol = 2, slices = c(3, 10), dimension = 3) %>% anatomy(v$anat, 0, 3000)
  expect_equal(lineAt(s3 %>% anatomySliceIndicator(v$anat, 0, 3000), "y"), c(2.5, 9.5))
  op <- options(RMINC_flip_image = FALSE); on.exit(options(op))
  expect_equal(lineAt(s3 %>% anatomySliceIndicator(v$anat, 0, 3000), "y"), c(25.5, 18.5))
})
