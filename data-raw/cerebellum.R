# Read the voxels that data-raw/cerebellum.py wrote, and store them as the
# internal `cerebellum` array (see data-raw/cerebellum.py for the source).
args <- commandArgs(trailingOnly = TRUE)
n <- 50L
cerebellum <- array(readBin(args[1], "integer", n^3, size = 4, endian = "little"),
                    dim = c(n, n, n))
save(cerebellum, file = "R/sysdata.rda", compress = "xz")
