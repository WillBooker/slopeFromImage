#' Find real coordinates of a lines object
#'
#' Function corrects the input spatial lines objects, from \code{\link{find_diff}}
#' into real space coordinates in order to calculate distance.
#'
#' @param pts Matrix or data.frame of line coordinates
#' @param xC Raster of corrected real space x values
#' @param yC Raster of corrected real space y values
#' @return Data frame of real coordinates
#' @export

find_real <- function(pts, xC, yC) {
  if (sum(is.na(pts))==0 & !any(pts == 0)) {
    xExt <- raster::extract(xC, pts) # real x value for each point
    yExt <- raster::extract(yC, pts) # real y value for each point
    realCoords <- data.frame(xExt+50, yExt)
    names(realCoords) <- c("x", "y")
    return(realCoords)
  } else {
    return(NA)
  }
}
