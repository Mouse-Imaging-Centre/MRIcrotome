# Small synthetic volumes shared by the tests, so no MINC files are needed.
testVolumes <- function() {
  set.seed(1)
  d <- c(20, 24, 28)
  anat <- array(runif(prod(d), 0, 1000), d)
  anat[6:15, 6:18, 6:22] <- anat[6:15, 6:18, 6:22] + 2000
  stats <- array(rnorm(prod(d)), d)
  stats[8:12, 10:14, 10:20] <- 5
  labels <- array(0L, d)
  labels[6:10, 6:10, 6:10] <- 1L
  labels[12:16, 12:16, 12:16] <- 2L
  list(anat = anat, stats = stats, labels = labels)
}
