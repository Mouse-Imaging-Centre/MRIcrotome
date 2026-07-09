# a set of functions to turn tag points into coordinates that can be
# passed to geom_point (or similar)

#' Title
#'
#' @param point a vector of length 3 in voxel coordinates
#' @param sliceList output of MRIcrotome
#'
#' @returns
#' @export
#'
#' @examples
convertPointToMRIcrotome <- function(point, sliceList, assemblySlice) {
  # account for shrinking to label bbox
  # TODO: double check that this is independent of dimension ordering
  point[1] <- point[1] - sliceList$labelExtents[[3]][1]
  point[2] <- point[2] - sliceList$labelExtents[[2]][1]
  point[3] <- point[3] - sliceList$labelExtents[[1]][1]
  
  # turn into a 2D vector
  # TODO: check that it works against different slice assemblies
  if (sliceList$sliceMod[[assemblySlice]][2] == 2)
    pointVec <- c(point[3] - sliceList$polBBoxes[[assemblySlice]]$xmin, point[1])
  else if (sliceList$sliceMod[[assemblySlice]][2] == 1)
    pointVec <- c(point[2] - sliceList$polBBoxes[[assemblySlice]]$xmin, point[1])
  # account for slice assemblies
  #browser()
  bboxes <- map_dbl(sliceList$polBBoxes, ~ c(.x$xmax - .x$xmin) + sliceList$sliceOffset)
  
  # and the final point to be returned
  if (assemblySlice > 1)
    finalPoint <- c(reduce(bboxes[1:(assemblySlice-1)], `+`) + pointVec[1], pointVec[2])
  else
    finalPoint <- c(pointVec[1], pointVec[2])
  return(data.frame(x=finalPoint[1], y=finalPoint[2]))
}

#' Title
#'
#' @param sliceDef 
#' @param pointCord 
#'
#' @returns
#' @export
#'
#' @examples
pointToSliceDistance <- function(sliceDef, pointCord) {
  sliceDef[1] - rev(pointCord)[sliceDef[2]]
}
#' Title
#'
#' @param sliceList 
#' @param pointCord 
#'
#' @returns
#' @export
#'
#' @examples
pointToSlicesDistance <- function(sliceList, pointCord) {
  map_dbl(sliceList, pointToSliceDistance, pointCord)
}