## Submission

This is a new submission.

MRIcrotome draws figures from 3D volumes such as MRI scans, with grid
graphics. It is the plotting partner of the RMINC package (on GitHub), but
it does not depend on it: it draws plain R arrays.

## Test environments

* GitHub Actions: Ubuntu with R-devel, R-release, R-oldrel-1 and R 4.1;
  macOS and Windows with R-release.
* Local: Linux, R 4.6.2.
* win-builder (R-devel): TO DO before submission.
* macOS builder: TO DO before submission.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jason Lerch <jason.lerch@sickkids.ca>'
  New submission

## Notes for the reviewers

* `R/sysdata.rda` (140 KB) holds a 50 x 50 x 50 voxel cut-out of a mouse
  brain MRI from the RMINC test data, which has the same copyright holder
  (Jason Lerch, cph in Authors@R) and licence (BSD-3-Clause) as this
  package. `exampleVolumes()` returns the data for the examples,
  tests and vignette. `data-raw/` (excluded from the build) shows how it was
  made.
* The examples write only to the current graphics device or to `tempfile()`.
