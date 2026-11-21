# a second layer never replaces the first (#46 item 1.2)
test_that("two contours() calls keep both contours", {
  v <- testVolumes()
  ssm <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>%
    contours(v$anat, levels = 1500, col = "red") %>%
    contours(v$stats, levels = 3, col = "blue")
  ss <- ssm$ssl[[1]]
  ord <- unlist(ss$order)
  expect_length(ord, 3)
  expect_equal(anyDuplicated(ord), 0)
  expect_equal(ord[2], "contours")
  expect_equal(ss$legendInfo[[ord[2]]]$col, "red")
  expect_equal(ss$legendInfo[[ord[3]]]$col, "blue")
  # a legend after the second call describes the second contours
  ssm <- ssm %>% legend("t")
  expect_equal(unlist(ssm$ssl[[1]]$legendOrder), ord[3])
})

test_that("two anatomy() calls in one series keep both", {
  v <- testVolumes()
  ss <- (sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>%
           anatomy(v$labels, 0, 2))$ssl[[1]]
  expect_length(unique(unlist(ss$order)), 2)
})

test_that("a name that is used or reserved is refused", {
  v <- testVolumes()
  base <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000)
  expect_error(base %>% overlay(v$stats, 2, 5, name = "anatomy"), "already in this slice series")
  expect_error(base %>% contours(v$anat, levels = 1500, name = "slices"), "cannot be used as a layer name")
  expect_error(base %>% overlay(v$stats, 2, 5, name = ""), "non-empty")
  ok <- base %>% overlay(v$stats, 2, 5, name = "ov") %>% contours(v$anat, levels = 1500, name = "c")
  expect_equal(unlist(ok$ssl[[1]]$order), c("anatomy", "ov", "c"))
})
