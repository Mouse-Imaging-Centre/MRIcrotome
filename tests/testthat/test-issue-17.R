test_that("defaultCol()/defaultRCol() are exported and follow the options (#17)", {
  expect_length(defaultCol(), 255)
  expect_length(defaultRCol(), 255)
  old <- options(MRIcrotomeCol = c("#000000", "#FFFFFF"), MRIcrotomeRcol = c("#FF0000"))
  on.exit(options(old))
  expect_equal(defaultCol(), c("#000000", "#FFFFFF"))
  expect_equal(defaultRCol(), "#FF0000")
})
