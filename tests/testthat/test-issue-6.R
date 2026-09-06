test_that("low/high default to the 2nd/98th percentiles (#6, #19)", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  expect_equal(getRange(1:100), c(2.98, 98.02))
  expect_equal(getRange(1:100, low = 5), c(5, 98.02))
  expect_equal(getRange(1:100, high = 50), c(2.98, 50))
  expect_equal(getRange(c(rep(0, 99), 5)), c(0, 5))   # sparse: full range instead of [0, 0]
  ssm <- sliceSeries(nrow = 2, ncol = 2, begin = 5, end = 20) %>%
    anatomy(v$anat) %>%
    overlay(v$labels, name = "ov") %>%
    legend("labels")
  expect_no_error(draw(ssm))
  li <- ssm$ssl[[1]]$legendInfo
  expect_equal(unlist(li$anatomy[c("low", "high")]), getRange(v$anat), ignore_attr = TRUE)
  expect_equal(li[["ov"]]$low, 0)
  expect_equal(li[["ov"]]$high, 2)
  # symmetric overlays take the range from the absolute values
  ssm <- sliceSeries(nrow = 1, begin = 5, end = 20) %>% anatomy(v$anat, 0, 3000) %>% overlay(v$stats, symmetric = TRUE, name = "ov")
  expect_equal(unlist(ssm$ssl[[1]]$legendInfo[["ov"]][c("low", "high")]),
               getRange(abs(v$stats)), ignore_attr = TRUE)
})
