# alpha works for every value and every colour format (#46 item 1.1)
test_that("setAlpha() sets the opacity of any colour", {
  expect_equal(setAlpha(c("red", "#0000FF", "#00FF0080"), 0.5),
               c("#FF00007F", "#0000FF7F", "#00FF007F"))
  expect_equal(setAlpha("red", 0.05), "#FF00000C")
  expect_equal(setAlpha("red", 0), "#FF000000")
  expect_error(setAlpha("red", 2), "between 0 and 1")
})

test_that("overlays with a small alpha or named colours draw", {
  v <- testVolumes()
  pdf(NULL); on.exit(dev.off())
  base <- sliceSeries(nrow = 1, ncol = 2) %>% anatomy(v$anat, 0, 3000)
  expect_no_error(draw(base %>% overlay(v$stats, 2, 5, alpha = 0.05)))
  expect_no_error(draw(base %>% overlay(v$stats, 2, 5, col = "red", alpha = 0.5)))
  expect_no_error(draw(base %>% overlay(v$stats, 2, 5, symmetric = TRUE,
                                        col = "red", rCol = "blue", alpha = 0.01)))
  # a volume of colour strings
  hex <- array(ifelse(v$labels > 0, "orange", NA_character_), dim(v$labels))
  ss <- (base %>% overlay(hex, alpha = 0.25))$ssl[[1]]
  r <- as.vector(ss[[ss$order[[2]]]][[2]][[1]]$raster)
  expect_true(all(is.na(r) | r == "#FFA5003F"))
})
