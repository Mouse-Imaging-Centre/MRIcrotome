# layout columns used by the titles, slice series, and legends of a row-layout grob
layoutCols <- function(g) {
  kind <- function(x) {
    if (inherits(x, "text")) "title"
    else if (inherits(x, "gTree") && inherits(x$children[[1]], "rect")) "slices"
    else "legend"
  }
  k <- sapply(g$children, function(ch) kind(ch$children[[1]]))
  split(unname(sapply(g$children, function(ch) ch$vp$layout.pos.col[1])), unname(k))
}

test_that("addtitle(side = ) places titles left or right in row layout (#16)", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  series <- function(ssm = NULL) {
    if (is.null(ssm)) sliceSeries(nrow = 1, ncol = 2, begin = 5, end = 20) %>% anatomy(v$anat, 0, 3000)
    else ssm %>% sliceSeries() %>% anatomy(v$anat, 0, 3000)
  }
  right <- series() %>% addtitle("A") %>% grobify(layout = "row")
  expect_equal(right$vp$layout$widths, unit(c(0.9, 0.1), "null"))
  expect_equal(layoutCols(right), list(slices = 1, title = 2))

  left <- series() %>% addtitle("A", side = "left") %>% grobify(layout = "row")
  expect_equal(left$vp$layout$widths, unit(c(0.1, 0.9), "null"))
  expect_equal(layoutCols(left), list(slices = 2, title = 1))

  mixed <- series() %>% addtitle("A", side = "left") %>%
    series() %>% addtitle("B") %>% legend("anat") %>%
    series() %>%
    grobify(layout = "row")
  expect_equal(mixed$vp$layout$widths, unit(c(0.1, 0.6, 0.1, 0.2), "null"))
  expect_equal(layoutCols(mixed), list(legend = 4, slices = c(2, 2, 2), title = c(1, 3)))
  expect_no_error(grid.draw(mixed))

  expect_error(series() %>% addtitle("A", side = "top"), "should be one of")
  # column layout is unaffected
  expect_no_error(series() %>% addtitle("A", side = "left") %>% draw())
})
