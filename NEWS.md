# MRIcrotome 0.3.0

This is the first release that is ready for CRAN. It contains all changes
since 0.2.0. The numbers are GitHub pull requests and issues.

## New features

* `exampleVolumes()` returns three volumes in one space for examples and
  tests: a real cut-out around the cerebellum of a mouse brain MRI (from the
  RMINC test data), a synthetic map of t-statistics, and labels for white and
  grey matter (#66). All examples and the vignette now run on them (#67, #68).
* `sliceSeries()` has `rotate` and `flip` arguments, which turn each slice of
  a series. All layers of the series stay aligned (#71, from #7).
* `figureSize()` calculates the device size at which the slices fill their
  area with no padding (#72, from #10).
* `maskBounds()` gives the first and last slice of a mask, for `begin` and
  `end` (#36, #5).
* `contours()` has a `labels` argument, to outline regions of a volume of
  labels. Its legend shows the label values (#35, #62, #11).
* `overlay()` has an `interpolate` argument; use `FALSE` for sharp label
  edges (#34, #18).
* `addtitle()` has a `side` argument for the row layout (#37, #16).
* `legend()` has `global = TRUE`, for one legend beside all rows in the row
  layout (#39, #15), and a `digits` argument (#63).
* `grobify()` and `draw()` have a `sliceBg` argument, to change or remove the
  black rectangle behind the slices. `draw()` gives other arguments to
  `grobify()` (#64).
* `defaultCol()` and `defaultRCol()` are exported, and the options
  `MRIcrotomeCol` and `MRIcrotomeRcol` change them (#29, #17).
* `%>%` is re-exported, so `library(MRIcrotome)` is enough for the examples
  (#65).
* When `low` or `high` is not given, the colour range is the 2nd to 98th
  percentile of the volume (#33, #6).

## Bug fixes

* More than one overlay in a slice series no longer stops with
  "subscript out of bounds" (#28, #22).
* A slice indicator now works without a legend (#31, #3).
* Legend labels stay on the page (#32, #8).
* Pipe steps return a copy and do not change their input, so one partial
  figure can be the start of more than one figure (#49).
* `anatomy()` with no volume slices the earlier anatomy again for the new
  series, so it follows that series' slices, rows, columns and dimension
  (#50).
* Two `contours()` (or `anatomy()`) layers in one series no longer replace
  each other. A layer name that is used or reserved gives an error (#51).
* `alpha` works for each value from 0 to 1 and for each colour format (#52).
* `anatomySliceIndicator()` uses `lineColour` (#53).
* Contours and slice indicator lines are on the voxels of the image. Before,
  they were half a voxel up and to the right. With
  `options(RMINC_flip_image = FALSE)`, they now flip with the image (#54).
* The lower end of the overlay colour scale is inclusive (#27).
* A second `legend()` on one layer changes that legend; it does not add
  another one. If one `legend()` of a series is global, the series' legend
  panel is global (#73).

## Error messages

* Clear errors for a file path or a `mincSingleDim` in place of a 3D array
  (#30, #14), a `dimension`, `nrow` or `ncol` that is not valid (#55), slices
  outside the volume (#57), a series with no layers and `legend()` with no
  layer (#58), a volume with only `NA` values (#59), and a `layout` that is
  not `"column"` or `"row"` (#61).
* A slice series with `slices` and only one of `nrow` and `ncol` calculates
  the other (#56).
* A slice indicator can come before the first layer (#58).
* `low == high` draws one colour with a warning; before, the layer was
  transparent (#60).

## Documentation

* The vignette builds from the package, with figures (#43, #68). A pkgdown
  site replaces the pre-rendered HTML (#69).
* The README documents the native pipe (#38), the slice order and
  orientation (#40), the device size (#41), and that RMINC is needed only to
  read MINC files (#76). The `legend()` help says that it masks
  `graphics::legend()` (#74).

## Package and checks

* `R CMD check --as-cran` passes with only the "New submission" note (#44).
  The package has a `LICENSE` file (#42).
* `DESCRIPTION` asks for R 2.10 or later, which the compressed example data
  needs. The package code needs nothing newer, but the current versions of
  purrr and testthat need R 4.1, so CI checks R 4.1 as the oldest version
  (#77).
* CI checks on Linux, macOS and Windows, and on R-devel, the current release,
  the previous release and R 4.1 (#25, #47, #75). The tests include visual
  snapshots (#48).
