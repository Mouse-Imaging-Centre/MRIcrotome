test_that("MRIcrotome package loads successfully", {
  expect_true("MRIcrotome" %in% loadedNamespaces())
})

test_that("sliceSeries returns expected structure", {
  ssm <- sliceSeries(nrow=2, ncol=2, begin=1, end=10)
  expect_true(is.environment(ssm))
  expect_true("ssl" %in% ls(ssm))
  ss <- ssm$ssl[[ssm$seriesCounter]]
  expect_s3_class(ss, "sliceSeries")
  expect_equal(ss$nrow, 2)
  expect_equal(ss$ncol, 2)
  expect_equal(ss$begin, 1)
  expect_equal(ss$end, 10)
  expect_equal(ss$dimension, 2)
})
