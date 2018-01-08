#' Creates polygon or surface lines
#'
#' Function creates a polygon of the difference between water and bed surfaces,
#' or returns them as lines objects.
#'
#' @param bsLoess List of bed surface loess objects output from \code{\link{fit_loess}}
#' @param wsLoess List of water surface loess objects output from \code{\link{fit_loess}}
#' @param i Iteration integer
#' @param out Integer (0 or 1) determining output type. 1 is polygon, 0 is spatial lines
#' @return Polygon of difference or lines of water/bed surface
#' @export

find_diff <- function(bsLoess, wsLoess, i, out) {
  #### Function returns coordinates of polygonal difference between the two profiles,
  #### if out == 1, or lines objects of surface profiles if out != 1

  wsDF <- data.frame(x = wsLoess[[1]][[i]][, 1], y =  predict(wsLoess[[2]][[i]])) # df of loess coords
  bdDF <- data.frame(x = bsLoess[[1]][[i]][, 1], y =  predict(bsLoess[[2]][[i]]))

  maxMin <- max(min(wsDF[, 1]), min(bdDF[, 1])) # lower x limit of polygons
  minMax <- min(max(wsDF[, 1]), max(bdDF[, 1])) # upper x limit of polygons

  wsDFf <- dplyr::filter(wsDF, (x <= minMax)&(x >= maxMin)) # only include values between above two
  if (nrow(wsDFf) == 0) {
    return('empty')
  }
  wsSP <- rbind(wsDFf, c(minMax, 0), c(maxMin, 0)) # draw polygon straight down
  wsSP <- sp::SpatialPolygons(list(Polygons(list(Polygon(wsSP)), 1)))
  wsSL <- sp::SpatialLines(list(Lines(list(Line(wsDFf)), ID = "1")))

  bsDFf <- dplyr::filter(bdDF, (x <= minMax)&(x >= maxMin))
  if (nrow(bsDFf) == 0) {
    return('empty')
  }
  bsSP <- rbind(bsDFf, c(minMax, 0), c(maxMin, 0))
  bsSP <- sp::SpatialPolygons(list(Polygons(list(Polygon(bsSP)), 1)))
  bsSL <- sp::SpatialLines(list(Lines(list(Line(bsDFf)), ID = "1")))

  x <- rgeos::gDifference(wsSP, bsSP) # difference between the two polygons
  if (is.null(x)) {
    return('nodepth')
  }
  xK <- lapply(x@polygons[[1]]@Polygons, function(x) {slot(x, "area")}) > 500 # only include areas larger than 500 pixels
  if (sum(xK) > 0) {
    xC <- lapply(x@polygons[[1]]@Polygons, function(x) {slot(x, "coords")})[xK]
  } else {
    xC <- NA
  }

  if (out == 1) {
    return(xC)
  } else {
    return(list(wsSL, bsSL))
  }

}
