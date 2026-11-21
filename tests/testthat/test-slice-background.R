# the rectangle behind the slices can be changed or removed (#46 item 3.4)
test_that("sliceBg sets or removes the slice background", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  ssm <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000)
  firstChild <- function(g) g$children[[1]]$children[[1]]$children[[1]]
  expect_equal(firstChild(grobify(ssm))$gp$fill, "black")
  expect_equal(firstChild(grobify(ssm, sliceBg = "white"))$gp$fill, "white")
  expect_false(inherits(firstChild(grobify(ssm, sliceBg = NA)), "rect"))
  expect_false(inherits(firstChild(grobify(ssm, layout = "row", sliceBg = NULL)), "rect"))
  # draw() gives other arguments to grobify()
  expect_no_error(draw(ssm, sliceBg = NA, bgCol = "white"))
  expect_error(draw(ssm, notAnArgument = 1), "unused argument")
})
