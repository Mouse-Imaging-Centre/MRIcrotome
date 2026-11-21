test_that("pipe steps do not change their input (#46 item 1.4)", {
  v <- testVolumes()
  base <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000)
  a <- base %>% overlay(v$stats, 2, 5, name = "a")
  b <- base %>% overlay(v$stats, 2, 5, name = "b") %>% legend("b") %>% addtitle("B")

  expect_equal(unlist(base$ssl[[1]]$order), "anatomy")
  expect_equal(unlist(a$ssl[[1]]$order), c("anatomy", "a"))
  expect_equal(unlist(b$ssl[[1]]$order), c("anatomy", "b"))
  expect_length(a$ssl[[1]]$legendOrder, 0)
  expect_length(a$ssl[[1]]$title, 0)

  # a new series and a slice indicator also leave the input as it was
  two <- base %>% sliceSeries() %>% anatomy()
  expect_equal(base$seriesCounter, 1)
  expect_length(base$ssl, 1)
  expect_equal(two$seriesCounter, 2)
  ind <- base %>% anatomySliceIndicator(v$anat, 0, 3000)
  expect_null(base$ssl[[1]]$sliceIndicator)
  expect_false(is.null(ind$ssl[[1]]$sliceIndicator))
})
