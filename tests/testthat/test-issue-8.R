test_that("the legend column is wide enough for its labels (#8)", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  lines <- function(u) convertWidth(u, "lines", valueOnly = TRUE)
  ssm <- sliceSeries(nrow = 2, ncol = 2, begin = 5, end = 20) %>%
    anatomy(v$anat, 0, 3000) %>%
    overlay(v$stats, low = 2.179, high = 3.055, symmetric = TRUE) %>%
    legend("t-statistics")
  ss <- ssm$ssl[[1]]
  needed <- lines(grobWidth(textGrob(legendStrings(2.179, 3.055, symmetric = TRUE))) + unit(3, "lines"))
  expect_gt(needed, 4)                         # the old fixed 4-line column clipped these labels
  expect_equal(lines(legendWidth(ss)), needed)
  expect_gt(lines(legendWidth(ss, gpar(fontsize = 24))), needed)
  g <- grobify(ssm)
  expect_equal(lines(g$vp$layout$widths[2]), needed)
  # a legend with short labels keeps the old 4-line minimum
  short <- sliceSeries(nrow = 1, begin = 5, end = 20) %>% anatomy(v$anat, 0, 3000) %>% legend()
  expect_equal(lines(legendWidth(short$ssl[[1]])), 4)
  # draw() leaves the viewport stack as it found it
  draw(ssm)
  expect_null(current.vpPath())
})
