# a legend of a label contour shows the labels, not 0.5 (#46 item 3.1)
test_that("label contour legends show the label values", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  legendText <- function(ssm) {
    g <- grobify(ssm)
    texts <- grid::getGrob(g, "contourLegend", grep = TRUE, global = TRUE)
    unname(unlist(lapply(texts$children, function(ch) if (inherits(ch, "text")) ch$label)))
  }
  one <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>%
    contours(v$labels, labels = 1) %>% legend("hippocampus")
  expect_equal(legendText(one), c("1", "hippocampus"))
  two <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>%
    contours(v$labels, labels = c(1, 2)) %>% legend()
  expect_equal(legendText(two), "1, 2")
  many <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>%
    contours(v$labels, labels = 1:5) %>% legend()
  expect_equal(legendText(many), "1, 2, 3, ...")
  # contours by level still show the levels
  lev <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>%
    contours(v$anat, levels = c(1500, 2500)) %>% legend()
  expect_equal(legendText(lev), c("1500", "2500"))
  expect_no_error(draw(many))
})
