# anatomySliceIndicator() uses lineColour (#46 item 1.7)
test_that("both slice indicators draw lines in lineColour", {
  v <- testVolumes()
  base <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000)
  lineCols <- function(ssm) {
    ind <- ssm$ssl[[1]]$sliceIndicator
    # one indicator line per slice, after the background grobs
    lines <- utils::tail(as.list(ind$children), 2)
    unique(vapply(lines, function(g) g$gp$col, character(1)))
  }
  expect_equal(lineCols(base %>% anatomySliceIndicator(v$anat, 0, 3000, lineColour = "red")), "red")
  expect_equal(lineCols(base %>% anatomySliceIndicator(v$anat, 0, 3000)), "green")
  expect_equal(lineCols(base %>% contourSliceIndicator(v$anat, 1500, lineColour = "blue")), "blue")
})
