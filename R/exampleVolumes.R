#' Example volumes for the examples and the vignette
#'
#' Returns three volumes in the same 50 x 50 x 50 voxel space, so that the
#' examples and the vignette run without MINC files or RMINC.
#'
#' @details \code{anatomy} is real data: a cut-out of a mouse brain MRI with
#'   0.056 mm voxels (the region looks like the cerebellum). It is
#'   \code{brain_cut_out.mnc} from the RMINC test data
#'   (\url{https://github.com/Mouse-Imaging-Centre/RMINC-test-data}),
#'   copyright Mouse Imaging Centre, Hospital for Sick Children, under the
#'   BSD-3-Clause licence; see the file \code{COPYRIGHTS} of the installed
#'   package. Its values go from about 190 to 2420. Dark tissue is about 600
#'   to 1030 and light tissue is above that.
#'
#'   \code{stats} is made up: a smooth map that looks like t-statistics, with
#'   one positive and one negative peak (about +6 and -5) on a low background.
#'
#'   \code{labels} divides \code{anatomy} by intensity only: 0 outside the
#'   tissue, 1 for dark tissue and 2 for light tissue. In this image, these are
#'   mostly white matter and grey matter, but they are not a segmentation.
#'
#'   The volumes are the same each time; the function does not use the random
#'   number generator.
#'
#' @return A list with the 3D arrays \code{anatomy}, \code{stats} and
#'   \code{labels}.
#' @export
#'
#' @examples
#' v <- exampleVolumes()
#' str(v)
#' sliceSeries(nrow = 2, ncol = 3, begin = 10, end = 40) %>%
#'   anatomy(v$anatomy, low = 600, high = 1400) %>%
#'   overlay(v$stats, low = 2, high = 6, symmetric = TRUE) %>%
#'   legend("t-statistics") %>%
#'   draw()
exampleVolumes <- function() {
  anat <- cerebellum + 0   # stored as integers; return doubles
  d <- dim(anat)
  x <- slice.index(anat, 1)
  y <- slice.index(anat, 2)
  z <- slice.index(anat, 3)
  blob <- function(centre, width)
    exp(-((x - centre[1])^2 + (y - centre[2])^2 + (z - centre[3])^2) / (2 * width^2))
  stats <- 6.5 * blob(c(18, 30, 32), 5) - 5.5 * blob(c(34, 20, 18), 4) +
    0.8 * sin(x / 3) * cos(y / 4) * sin(z / 5)
  labels <- array(0L, d)
  labels[anat >= 600] <- 1L
  labels[anat >= 1030] <- 2L
  list(anatomy = anat, stats = stats, labels = labels)
}
