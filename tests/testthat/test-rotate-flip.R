# sliceSeries(rotate =, flip =) turns every layer of a series (from issue #7)
test_that("orientSlice() rotates counterclockwise and mirrors", {
  m <- matrix(1:6, nrow = 3)   # 3 wide, 2 high; m[1, 2] is the top-left voxel
  r90 <- orientSlice(m, 90)
  expect_equal(dim(r90), c(2, 3))
  expect_equal(r90[1, 1], m[1, 2])   # top-left goes to bottom-left
  r270 <- orientSlice(m, 270)
  expect_equal(r270[2, 3], m[1, 2])  # top-left goes to top-right
  expect_equal(orientSlice(m, 180), m[3:1, 2:1])
  expect_equal(orientSlice(orientSlice(m, 90), 270), m)
  expect_equal(orientSlice(m, 0, "horizontal"), m[3:1, ])
  expect_equal(orientSlice(m, 0, "vertical"), m[, 2:1])
  expect_equal(orientSlice(m, 90, "horizontal"), r90[2:1, ])
})

# native bounding box of the pixels that differ from the corner pixel
rasterBox <- function(g) {
  m <- as.matrix(g$raster)
  on <- m != m[1, 1]
  cols <- range(which(apply(on, 2, any)))
  rows <- range(which(apply(on, 1, any)))
  list(x = c(cols[1] - 1, cols[2]), y = sort(nrow(m) - rows + c(1, 0)))
}
contourBox <- function(g) list(x = range(unlist(lapply(g, function(l) as.numeric(l$x)))),
                               y = range(unlist(lapply(g, function(l) as.numeric(l$y)))))

test_that("rotated contours stay on the rotated image", {
  vol <- array(0, c(20, 24, 28))
  vol[2:4, , 10:20] <- 1      # in a dimension-2 slice: x 2..4, y 10..20
  for (rot in c(0, 90, 180, 270)) for (fl in c("none", "horizontal", "vertical")) {
    img <- sliceImage(vol, 2, 5, 0, 1, rotate = rot, flip = fl)[[1]]
    con <- sliceContours(vol, 2, 5, levels = 0.5, rotate = rot, flip = fl)
    expect_equal(contourBox(con), rasterBox(img), info = paste(rot, fl))
  }
  expect_equal(rasterBox(sliceImage(vol, 2, 5, 0, 1, rotate = 90)[[1]]),
               list(x = c(8, 19), y = c(1, 4)))
})

test_that("a rotated series swaps its layout and keeps its settings", {
  v <- testVolumes()   # dimension 2 slices are 20 wide and 28 high
  pdf(NULL); on.exit(dev.off())
  ssm <- sliceSeries(nrow = 1, ncol = 2, rotate = 90) %>% anatomy(v$anat, 0, 3000) %>%
    contours(v$labels, labels = 1)
  ss <- ssm$ssl[[1]]
  lay <- ss$seriesVP$layout
  expect_equal(as.numeric(lay$widths), c(28, 28))
  expect_equal(as.numeric(lay$heights), 20)
  expect_equal(dim(as.matrix(ss$anatomy[[1]][[1]]$raster)), c(20, 28))
  expect_no_error(draw(ssm))
  # carried over to the next series unless given again
  two <- ssm %>% sliceSeries(flip = "vertical") %>% anatomy()
  expect_equal(two$ssl[[2]]$rotate, 90)
  expect_equal(two$ssl[[2]]$flip, "vertical")
  expect_equal(sliceSeries(nrow = 1)$ssl[[1]][c("rotate", "flip")], list(rotate = 0, flip = "none"))
})

test_that("rotate and flip are checked", {
  expect_error(sliceSeries(nrow = 1, rotate = 45), "0, 90, 180 or 270")
  expect_error(sliceSeries(nrow = 1, flip = "diagonal"), "should be one of")
  expect_equal(sliceSeries(nrow = 1, flip = "h")$ssl[[1]]$flip, "horizontal")
})
