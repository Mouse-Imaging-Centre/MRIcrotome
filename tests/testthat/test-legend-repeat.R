# legend() two times on one layer, and the global rule (#46 item 3.2)
test_that("a second legend() on the same layer replaces the first", {
  v <- testVolumes()
  ssm <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>%
    overlay(v$stats, 2, 5, name = "ov") %>% legend("a") %>% legend("b", digits = 4)
  ss <- ssm$ssl[[1]]
  expect_equal(unlist(ss$legendOrder), "ov")
  expect_equal(ss$legendInfo$ov$description, "b")
  expect_equal(ss$legendInfo$ov$digits, 4)
})

test_that("one global legend makes the side panel of the series global", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  ssm <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>%
    overlay(v$stats, 2, 5) %>% legend("t", global = TRUE) %>%
    contours(v$anat, levels = 1500) %>% legend("c")
  expect_true(ssm$ssl[[1]]$legendGlobal)
  expect_false((sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>%
                  legend("a"))$ssl[[1]]$legendGlobal)
  two <- ssm %>% sliceSeries() %>% anatomy()
  g <- grobify(two, layout = "row")
  # grid keeps layout.pos.row/col as a range c(first, last)
  panel <- Filter(function(ch) all(ch$vp$layout.pos.col == 2), g$children)
  expect_length(panel, 1)
  expect_equal(as.numeric(panel[[1]]$vp$layout.pos.row), c(1, 2))
})
