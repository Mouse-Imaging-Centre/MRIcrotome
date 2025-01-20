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

combineTwoPolsX <- function(pols2, pols1, offset=0) {
  pols1Extents <- attr(pols1, "sliceExtents")
  pols2Extents <- attr(pols2, "sliceExtents")
  st_geometry(pols1) <- st_geometry(pols1) + c(pols2Extents[2]+offset, 0)
  cpols <- rbind(pols1, pols2)
  attr(cpols, "sliceExtents") <- 
    ext(pols2Extents[1] + pols1Extents[2] + offset, 
        pols2Extents[2] + pols1Extents[2] + offset, 
        pols2Extents[3],
        pols2Extents[4])
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
combineTwoSlicesX <- function(slice1, slice2, offset=0) {
  ext(slice2) <- ext( xmax(slice1) + offset,
                    (xmax(slice1) + xmax(slice2)) + offset,
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

shrinkPolsToBBox <- function(pols, bbox, assembleDir) {
  originalExt <- attr(pols, "sliceExtents")
  if (assembleDir == "X") {
    st_geometry(pols) <- st_geometry(pols) - c( bbox[1], 0 )
    attr(pols, "sliceExtents") <- ext(0, bbox[3]-bbox[1], originalExt[3], originalExt[4])
  }
  return(pols)
}
shrinkSliceToBBox <- function(slice, bbox, assembleDir) {
  originalExt <- ext(slice)
  if (assembleDir == "X") {
    croppedSlice <- crop(slice, 
                         ext(bbox[1],
                             bbox[3],
                             originalExt[3],
                             originalExt[4]))
    ext(croppedSlice) <- ext(0, bbox[3]-bbox[1], originalExt[3], originalExt[4])
  }
  else if (assembleDir == "Y") {
    return(crop(slice,
                ext(originalExt[1],
                    originalExt[2],
                    bbox[2],
                    bbox[4])))
  }
  return(croppedSlice)
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

assembleSlicesAndPols <- function(anatSlices, labelPols=NULL, assembleDir="Y", centreSlices=T, polBBoxes) {

  # shrink anatomy to label bounding boxes
  anatSlices <- map2(anatSlices, polBBoxes, 
                     ~ shrinkSliceToBBox(.x, .y, assembleDir))
  if (!is.null(labelPols)) {
    # shrink polygons to label bounding boxes
    labelPols <- map2(labelPols, polBBoxes,
                    ~ shrinkPolsToBBox(.x, .y, assembleDir))
  }
  if (assembleDir == "Y") {
    if (centreSlices) {
      # centre first
      xadditions <- getXAdditions(anatSlices)
      anatSlices <- centreSlicesY(anatSlices, xadditions)
      if (!is.null(labelPols)) {
        labelPols <- centrePolsY(labelPols, xadditions)
      }
      
      #fList <- c(fList, function(x) centreSlicesY(x, xadditions))
    }  
    # then assemble
    assembledSlices <- reduce(anatSlices, combineTwoSlicesY)
    if (!is.null(labelPols)) {
      assembledPols <- reduce(labelPols, combineTwoPolsY)
    }
    
    #fList <- c(fList, function(x) reduce(x, combineTwoSlicesY))
  }
  else if (assembleDir == "X") {
    if (centreSlices) {
      #todo
    }
    assembledSlices <- reduce(anatSlices, combineTwoSlicesX)
    if (!is.null(labelPols)) {
      assembledPols <- reduce(labelPols, combineTwoPolsX)
    }
    
    #fList <- c(fList, function(x) reduce(x, combineTwoSlicesX))
  }
  if (!is.null(labelPols)) {
    return(list(assembledSlices, assembledPols))
  }
  else {
    return(assembledSlices)
  }
}

makeSliceListObject <- function(anatomy, labels, labelDefs, sliceList, assembleDir="Y", centreSlices=T ) {
  # a list of functions for adding new volumes to the layout later
  fList <- list()
  
  # redefine labels (makes it easier for pruned trees)
  labelDefs$Set(.id=1:length(labelDefs$Get("name")))
  labels <- hanatToVolume(labelDefs, labels, ".id")
  
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
  # check if a third entry is provided in the slice List for all slices. If not,
  # assemble in the specified direction
  if (any(is.na(map_dbl(sliceList, ~ .x[3])))) {

    #fList <- c(fList, function(x) map2(x, polBBoxes,
    #                                   ~ shrinkSliceToBBox(.x, .y, assembleDir)))
    assembled <- assembleSlicesAndPols(anatSlices, labelPols, assembleDir, centreSlices, polBBoxes)
    fList <- c(fList, function(x) assembleSlicesAndPols(x, NULL, assembleDir, centreSlices, polBBoxes))
  }
  # if a third entry is provided in the slice list, then assemble each row in X
  # and the rows themselves in Y.
  else {
    sliceRows <- map_dbl(sliceList, ~ .x[3])
    
    polBBoxesList <- split(polBBoxes, sliceRows)
    
    anatSliceList <- split(anatSlices, sliceRows)
    fList <- c(fList, function(x) split(x, sliceRows))
    labelPolList <- split(labelPols, sliceRows)

    assemblies <- pmap(list(anatSliceList, labelPolList, polBBoxesList), ~ 
                         assembleSlicesAndPols(..1, ..2, assembleDir="X", centreSlices=F, ..3))
    polBBoxes <- map(map(assemblies, ~ .x[[2]]), st_bbox)
    assembled <- assembleSlicesAndPols(map(assemblies, ~ .x[[1]]), 
                                       map(assemblies, ~ .x[[2]]), 
                                       assembleDir = "Y", centreSlices = T, polBBoxes)
    fList <- c(fList, function(x) map2(x, polBBoxesList, ~ assembleSlicesAndPols(.x, NULL, assembleDir="X", centreSlices=F, .y)))
    fList <- c(fList, function(x) assembleSlicesAndPols(x, NULL, assembleDir = "Y", centreSlices = T, polBBoxes))
      
  }
  
  # incorporate palette and labels from the anatomical tree
  #labelDefs$Set(.id=1:length(labelDefs$Get("name")))
  #treeData <- ToDataFrameTable(labelDefs, "name", "label", "color_hex_triplet", "acronym") %>%
  #  filter(label %in% assembled[[2]]$lyr.1)
  treeData <- left_join(data.frame(.id=assembled[[2]]$lyr.1),
                        ToDataFrameTable(labelDefs, "name", "label", ".id", "color_hex_triplet", "acronym"))
  
  palette <- setNames(treeData$color_hex_triplet, treeData$name)
  
  outdata <- assembled[[2]] %>%
    mutate(
      region = treeData$name,
      hemi="left",
      side="coronal",
      label=treeData$name,
      slice="someslice",
      acronym=treeData$acronym
    )
  
  class(outdata) <- c("sf", "brain_data", "ggseg_atlas", "tbl_df", "tbl", "data.frame")
  
  out <- 
    list(
      atlas = "some atlas",
      type = "subcortical",
      anatomy=assembled[[1]],
      data=outdata,
      sliceMod=modSliceList,
      functions=fList,
      palette=palette
    )
  class(out) <- "brain_atlas"
  return(out)
  
}

applyFuncsSeq <- function(volume, fList) {
  out <- fList[[1]](volume)
  for (i in 2:length(fList)) {
    out <- fList[[i]](out)
  }
  return(out)
}