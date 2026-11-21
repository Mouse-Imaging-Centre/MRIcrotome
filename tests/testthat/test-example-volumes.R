test_that("exampleVolumes() gives three volumes in one space, the same each time", {
  seed <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv())
  v <- exampleVolumes()
  expect_named(v, c("anatomy", "stats", "labels"))
  for (a in v) expect_equal(dim(a), c(50, 50, 50))
  expect_equal(range(v$anatomy), c(189, 2419))
  expect_true(max(v$stats) > 6 && min(v$stats) < -5)
  expect_setequal(unique(as.vector(v$labels)), 0:2)
  expect_identical(exampleVolumes(), v)
  # it does not use the random number generator
  expect_identical(if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()), seed)
})

test_that("the example volumes draw", {
  v <- exampleVolumes()
  pdf(NULL); on.exit(dev.off())
  expect_no_error(sliceSeries(nrow = 1, ncol = 3, begin = 10, end = 40) %>%
                    anatomy(v$anatomy, 600, 1400) %>%
                    overlay(v$stats, 2, 6, symmetric = TRUE) %>%
                    contours(v$labels, labels = 1, col = "white") %>%
                    draw())
})
