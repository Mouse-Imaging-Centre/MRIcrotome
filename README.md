# MRIcrotome

MRIcrotome provides tools to create publication-ready figures from 3D
volumes such as MRI data. It uses a pipe-based interface built on R's
`grid` graphics system to build up slice series with anatomy underlays,
statistical overlays, contours, legends, and titles.

## Installation

MRIcrotome depends on [RMINC](https://github.com/Mouse-Imaging-Centre/RMINC).
Install RMINC first, then install MRIcrotome with `devtools`:

```r
# install.packages("devtools")
devtools::install_github("gdevenyi/MRIcrotome")
```

## Usage

MRIcrotome is designed to be used with pipes. A typical workflow starts with
`sliceSeries()`, adds layers such as `anatomy()`, `overlay()`, or
`contours()`, optionally adds `legend()` and `addtitle()`, and finishes with
`draw()`.

```r
library(MRIcrotome)
library(grid)

# Simple anatomy display
sliceSeries(nrow = 1, begin = 200, end = 300) %>%
  anatomy(anatVol, low = 700, high = 1400) %>%
  draw()
```

### Overlays and legends

```r
sliceSeries(nrow = 5, ncol = 5, begin = 100, end = 350) %>%
  anatomy(anatVol, low = 700, high = 1400) %>%
  overlay(stats, low = 2, high = 6, symmetric = TRUE) %>%
  legend("t-statistics") %>%
  draw()
```

### Multiple slice series with titles

```r
sliceSeries(nrow = 8, begin = 100, end = 350) %>%
  anatomy(anatVol, low = 700, high = 1400) %>%
  addtitle("Anatomy") %>%
  sliceSeries() %>%
  anatomy() %>%
  overlay(stats, low = 2, high = 6, symmetric = TRUE) %>%
  addtitle("Stats") %>%
  legend("t-statistics") %>%
  draw()
```

### Contours

```r
sliceSeries(nrow = 1, begin = 200, end = 300) %>%
  anatomy(anatVol, low = 700, high = 1400) %>%
  overlay(stats, low = 2, high = 6, symmetric = TRUE) %>%
  legend("t-statistics") %>%
  contours(abs(stats), levels = c(3, 5), lwd = 2, lty = c(3, 1), col = "green") %>%
  legend("Most sig.") %>%
  draw()
```

### Slice indicators

```r
sliceSeries(nrow = 5, begin = 150, end = 250) %>%
  anatomy(anatVol, low = 700, high = 1400) %>%
  overlay(stats, low = 2, high = 6, symmetric = TRUE, alpha = 0.5) %>%
  legend("t-statistics") %>%
  anatomySliceIndicator(anatVol, 700, 1400) %>%
  draw()
```

### Using grobify for grid integration

Use `grobify()` instead of `draw()` to obtain a `gTree` object that can
be incorporated into other `grid` graphics:

```r
g <- sliceSeries(nrow = 5, begin = 150, end = 250) %>%
  anatomy(anatVol, low = 700, high = 1400) %>%
  overlay(stats, low = 2, high = 6, symmetric = TRUE) %>%
  legend("t-statistics") %>%
  grobify()

grid.newpage()
grid.draw(g)
```

## Key functions

| Function | Description |
|---|---|
| `sliceSeries()` | Initialize a slice series grid |
| `anatomy()` | Add anatomy underlay slices |
| `overlay()` | Add statistical or other overlay slices |
| `contours()` | Add contour lines |
| `legend()` | Add a colour-bar or contour legend |
| `addtitle()` | Add a title to a slice series |
| `draw()` | Render the figure to the current device |
| `grobify()` | Convert the figure to a `gTree` for grid integration |
| `anatomySliceIndicator()` | Add an anatomy-based slice position indicator |
| `contourSliceIndicator()` | Add a contour-based slice position indicator |

## License

BSD 3-Clause. See the `DESCRIPTION` file for details.
