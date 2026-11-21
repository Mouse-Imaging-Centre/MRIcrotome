# library(MRIcrotome) is enough to use %>% (#46 item 3.5)
test_that("the pipe is exported", {
  expect_true("%>%" %in% getNamespaceExports("MRIcrotome"))
  expect_identical(MRIcrotome::`%>%`, purrr::`%>%`)
})
