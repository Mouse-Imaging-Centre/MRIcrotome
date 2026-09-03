test_that("contours(labels = ) outlines label regions (#11)", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  ssm <- function() sliceSeries(nrow = 1, begin = 8, end = 8) |> anatomy(v$anat, low = 0, high = 3000)
  g <- ssm() |> contours(v$labels, labels = 1, col = "magenta") |> grobify()
  expect_no_error(grid.draw(g))
  expect_equal(ssm() |> contours(v$labels, labels = 1) |> getElement("ssl") |> (\(x) x[[1]]$legendInfo$contours$levels)(), 0.5)
  # positional use of col still works, and misuse is reported
  expect_no_error(ssm() |> contours(v$stats, 3, "green"))
  expect_error(ssm() |> contours(v$labels, levels = 0.5, labels = 1), "both")
  expect_error(ssm() |> contours(v$labels), "either")
})
