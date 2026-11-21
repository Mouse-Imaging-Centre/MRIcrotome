# legend numbers are rounded to `digits` significant digits (#46 item 3.3)
test_that("legendStrings() rounds to digits but keeps whole numbers", {
  expect_equal(trimws(legendStrings(-2.0155497, 5)), c("-2.02", "5"))
  expect_equal(trimws(legendStrings(-2.0155497, 5, digits = 5)), c("-2.0155", "5"))
  expect_equal(trimws(legendStrings(700, 1400)), c("700", "1400"))
  expect_equal(trimws(legendStrings(1234.5678, 2000)), c("1235", "2000"))
  expect_equal(trimws(legendStrings(2, 6, symmetric = TRUE)), c("2", "6", "-2", "-6"))
})

test_that("legend(digits = ) reaches the drawn colour bar", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  labels <- function(ssm) {
    g <- grid::getGrob(grobify(ssm), "legend", grep = TRUE, global = TRUE)
    trimws(unname(unlist(lapply(g$children, function(ch) if (inherits(ch, "text")) ch$label))))
  }
  base <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000) %>% overlay(v$stats)
  expect_equal(labels(base %>% legend("t"))[1:2], c("-2.02", "5"))
  expect_equal(labels(base %>% legend("t", digits = 5))[1:2], c("-2.0155", "5"))
})
