test_that("several overlays on one slice series can be drawn (#22)", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    sliceSeries(nrow = 2, ncol = 2, begin = 5, end = 20) |>
      anatomy(v$anat, low = 0, high = 3000) |>
      overlay(v$stats, low = 0, high = 6, symmetric = TRUE) |>
      overlay(v$stats, low = 2, high = 6, symmetric = TRUE) |>
      legend("t-statistics") |>
      draw()
  )
  ssm <- sliceSeries(nrow = 1, begin = 5, end = 20) |>
    anatomy(v$anat, low = 0, high = 3000) |>
    overlay(v$stats, low = 0, high = 6) |>
    overlay(v$stats, low = 2, high = 6) |>
    legend("second")
  ss <- ssm$ssl[[1]]
  expect_length(unique(unlist(ss$order)), 3)
  expect_equal(ss$legendInfo[[ss$order[[3]]]]$description, "second")
  # a legend on re-used anatomy gets the original colour range
  ssm <- sliceSeries(nrow = 1, begin = 5, end = 20) |> anatomy(v$anat, low = 100, high = 900) |>
    sliceSeries() |> anatomy() |> legend("anatomy")
  expect_equal(ssm$ssl[[2]]$legendInfo$anatomy$high, 900)
  expect_no_error(draw(ssm))
})
