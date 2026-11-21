"""Make R/sysdata.rda's `cerebellum` from the RMINC test data.

Source: rminctestdata/brain_cut_out.mnc in
https://github.com/Mouse-Imaging-Centre/RMINC-test-data, copyright (c) 2022
Mouse Imaging Centre, Hospital for Sick Children, BSD-3-Clause (the notice is
in inst/COPYRIGHTS). It is a 50 x 50 x 50 voxel (0.056 mm) cut-out of a mouse
brain MRI; the region looks like the cerebellum.

MINC2 files are HDF5, so h5py reads them without the MINC toolkit:

    uv venv && uv pip install h5py numpy
    python data-raw/cerebellum.py brain_cut_out.mnc cerebellum.bin
    Rscript data-raw/cerebellum.R cerebellum.bin

The voxels are stored as uint16 with one real range per z slice; this
script converts them to real values and rounds them to whole numbers,
which loses nothing visible (the range is about 190 to 2420).
"""
import sys
import h5py
import numpy as np

src, out = sys.argv[1], sys.argv[2]
with h5py.File(src, "r") as f:
    g = f["minc-2.0/image/0"]
    img = g["image"]
    assert img.attrs["dimorder"] == b"zspace,yspace,xspace"
    lo, hi = img.attrs["valid_range"]
    a = img[...].astype(float)
    mn = g["image-min"][...][:, None, None]
    mx = g["image-max"][...][:, None, None]
    real = (a - lo) / (hi - lo) * (mx - mn) + mn
# C order of (z, y, x) is the Fortran order of (x, y, z): the order of
# RMINC's mincArray(), and of an R array with dim c(50, 50, 50)
np.round(real).astype("<i4").tofile(out)
