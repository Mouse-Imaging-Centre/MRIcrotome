# returns the maximum non-zero extents of a particular dimension
# of a volume
labelVolExtentsDim <- function(labelVol, dimension) {
  labelVoxels <- apply(labelVol, dimension, function(x) any(x>0.5))
  labelVrange <- range((1:length(labelVoxels))[labelVoxels])
  return(labelVrange)
}


# figure out the maximum non-zero extents of a volume
# used for cropping purposes
labelVolExtents <- function(labelVol) {
  map(1:3, ~labelVolExtentsDim(labelVol, .x))
}

# takes a volume and shrinks it according to previously determined extents
shrinkVolumeToExtents <- function(vol, labelExtents) {
  vol[labelExtents[[1]][1]:labelExtents[[1]][2],
      labelExtents[[2]][1]:labelExtents[[2]][2],
      labelExtents[[3]][1]:labelExtents[[3]][2]]
}

# takes a slice definition and modifies it based on previously determined extents
# This is useful if the slice def is in the coordinates of the full volume
# but the volume has been shrunk to label extents
modifySliceListEntry <- function(sliceListEntry, labelExtents) {
  c(sliceListEntry[1] - labelExtents[[sliceListEntry[2]]][1], sliceListEntry[2])
}

# modifies all slice defs in a sliceList
modifySliceList <- function(sliceList, labelExtents) {
  map(sliceList, modifySliceListEntry, labelExtents)
}

# create a raster of a single slice in a volume
getSliceRast <- function(slice, vol, makeZeroNA=FALSE) {
  d <- dim(vol)
  slicen <- slice[1]
  slicedim <- slice[2]
  if (slicedim == 1) {
    s1 <- t(vol[slicen,,d[3]:1])
  } else if (slicedim == 2) {
    s1 <- t(vol[,slicen,d[3]:1]) # FIXME correct dim choice
  } else if (slicedim == 3) {
    s1 <- t(vol[,d[2]:1,slicen])
  }
  if (makeZeroNA) {
    s1[s1<0.5] <- NA
  }
  return(rast(s1))
}

# takes a slice of a label volume and creates and smoothes polygons
sliceToPolygons <- function(sRast, smoothness=1.5) {
  p1 <- as.polygons(sRast)
  pols <- st_as_sf(p1)
  spols <- smooth(pols, method="ksmooth", smoothness=smoothness) # TODO: make choices optional  
  attr(spols, "sliceExtents") <- ext(sRast)
  return(spols)
}

# combines two sets of polygons in Y
combineTwoPolsY <- function(pols1, pols2) {
  pols2Extents <- attr(pols2, "sliceExtents")
  st_geometry(pols1) <- st_geometry(pols1) + c(0, pols2Extents[4])
  cpols <- rbind(pols2, pols1)
  attr(cpols, "sliceExtents") <- 
    ext(pols2Extents[1], 
        pols2Extents[2], 
        pols2Extents[3] + pols2Extents[4],
        pols2Extents[4] + pols2Extents[4])
  return(cpols)
}

# combines two sets of polygons in X
combineTwoPolsX <- function(pols1, pols2) {
  pols1Extents <- attr(pols1, "sliceExtents")
  st_geometry(pols2) <- st_geometry(pols2) + c(pols1Extents[2], 0)
  cpols <- rbind(pols2, pols1)
  attr(cpols, "sliceExtents") <- 
    ext(pols1Extents[1] + pols1Extents[2], 
        pols1Extents[2] + pols1Extents[2], 
        pols1Extents[3],
        pols1Extents[4])
  return(cpols)
}

# combines two sets of slices in Y
combineTwoSlicesY <- function(slice1, slice2) {
  ext(slice1) <- c( xmin(slice1),
                    xmax(slice1),
                    ymax(slice2),
                    ymax(slice1) + ymax(slice2) )
  merge(slice2, slice1)
}

# combines two sets of slices in X
combineTwoSlicesX <- function(slice1, slice2) {
  ext(slice2) <- c( xmax(slice1),
                    xmax(slice1) + xmax(slice2),
                    ymin(slice2),
                    ymax(slice2) )
  merge(slice2, slice1)
}

cropSliceToBBox <- function(slice, bbox) {
  crop(slice, 
       ext(bbox[1],
           bbox[3],
           bbox[2],
           bbox[4]))
}

centreSliceY <- function(slice, xaddition) {
  ext(slice) <- c(xaddition, 
                  xmax(slice) + xaddition, 
                  ymin(slice), 
                  ymax(slice))
  return(slice)
}

centrePolY <- function(pol, xaddition) {
  st_geometry(pol) <- st_geometry(pol) + c(xaddition, 0)
  return(pol)
}

centrePolsY <- function(pols, xadditions) {
  map2(pols, xadditions, centrePolY)
}

getXAdditions <- function(slices) {
  xmaxes <- map_dbl(slices, xmax)
  as.list((max(xmaxes) - xmaxes) / 2)
}

centreSlicesY <- function(slices, xadditions) {
  map2(slices, xadditions, centreSliceY)
}

makeSliceListObject <- function(anatomy, labels, labelDefs, sliceList, assembleDir="Y", centreSlices=T ) {
  # a list of functions for adding new volumes to the layout later
  fList <- list()
  
  # shrink volume to label extents
  labelExtents <- labelVolExtents(labels)
  shrunkAnat <- shrinkVolumeToExtents(anatomy, labelExtents)
  shrunkLabels <- shrinkVolumeToExtents(labels, labelExtents)
  
  fList <- c(fList, function(x) shrinkVolumeToExtents(x, labelExtents))
  
  # modify sliceList to reflect new label extents
  modSliceList <- modifySliceList(sliceList, labelExtents)
  
  # iterate over slice list to create anatomy slices
  anatSlices <- map(modSliceList, getSliceRast, shrunkAnat)
  fList <- c(fList, function(x) map(modSliceList, getSliceRast, x))
  
  # iterate over sliceList to create polygons
  labelSlices <- map(modSliceList, getSliceRast, shrunkLabels, makeZeroNA=T)
  labelPols <- map(labelSlices, sliceToPolygons)
  
  # get bounding boxes of geometry for further size modifications
  polBBoxes <- map(labelPols, st_bbox)
  
  ## assemble the slices
  
  if (assembleDir == "Y") {
    if (centreSlices) {
      # centre first
      xadditions <- getXAdditions(anatSlices)
      anatSlices <- centreSlicesY(anatSlices, xadditions)
      labelPols <- centrePolsY(labelPols, xadditions)
      
      fList <- c(fList, function(x) centreSlicesY(x, xadditions))
    }  
    # then assemble
    assembledSlices <- reduce(anatSlices, combineTwoSlicesY)
    assembledPols <- reduce(labelPols, combineTwoPolsY)
    
    fList <- c(fList, function(x) reduce(x, combineTwoSlicesY))
  }
  else if (assembleDir == "X") {
    if (centreSlices) {
      #todo
    }
    assembledSlices <- reduce(anatSlices, combineTwoSlicesX)
    assembledPols <- reduce(labelPols, combineTwoPolsX)
    
    fList <- c(fList, function(x) reduce(x, combineTwoSlicesX))
  }
  return(
    list(
      assembledSlices,
      assembledPols,
      modSliceList,
      fList
    )
  )
  
}

applyFuncsSeq <- function(volume, fList) {
  out <- fList[[1]](volume)
  for (i in 2:length(fList)) {
    out <- fList[[i]](out)
  }
  return(out)
}