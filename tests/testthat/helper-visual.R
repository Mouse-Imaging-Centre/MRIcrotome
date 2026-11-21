# Visual snapshots. The SVG files keep real images, so that
# testthat::snapshot_review() shows the figures, but two files are compared on
# their decoded pixels: the zlib compression of the embedded PNGs differs
# between machines even when every pixel is the same.

base64Decode <- function(s) {
  alphabet <- c(LETTERS, letters, 0:9, "+", "/")
  v <- match(strsplit(gsub("=", "", s), "")[[1]], alphabet) - 1L
  bits <- vapply(v, function(x) as.integer(intToBits(x))[6:1], integer(6))
  bits <- as.vector(bits)
  bits <- bits[seq_len(length(bits) %/% 8 * 8)]
  as.raw(colSums(matrix(bits, nrow = 8) * 2^(7:0)))
}

# header and decompressed image data of a PNG; drop the compressed bytes
pngPixels <- function(png) {
  i <- 9L
  header <- raw(); idat <- raw()
  while (i < length(png)) {
    n <- readBin(png[i:(i + 3L)], "integer", size = 4, endian = "big")
    type <- rawToChar(png[(i + 4L):(i + 7L)])
    body <- if (n > 0) png[(i + 8L):(i + 7L + n)] else raw()
    if (type == "IHDR") header <- body
    if (type == "IDAT") idat <- c(idat, body)
    i <- i + 12L + n
  }
  list(header = header, pixels = memDecompress(idat, type = "gzip"))
}

normaliseSvg <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  pattern <- "data:image/png;base64,[A-Za-z0-9+/=]+"
  uris <- regmatches(text, gregexpr(pattern, text))[[1]]
  images <- lapply(sub("^data:image/png;base64,", "", uris),
                   function(u) pngPixels(base64Decode(u)))
  list(text = gsub(pattern, "PNG", text), images = images)
}

compareSvgPixels <- function(old, new) {
  identical(normaliseSvg(old), normaliseSvg(new))
}

expect_figure <- function(title, fig) {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  path <- tempfile(fileext = ".svg")
  vdiffr::write_svg(fig, path, title)
  expect_snapshot_file(path, paste0(title, ".svg"), compare = compareSvgPixels)
}
