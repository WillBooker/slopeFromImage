#' Return regression value or coordinates for boundary
#'
#' Function takes the input boundary raster and returns either the regression values
#' for the boundary or the pixel/real coordinates of the points.
#'
#' @param boundaryObj Raster of boundary values taken from \code{\link{find_boundary}}
#' @param xCor Raster of corrected real space x values
#' @param yCor Raster of corrected real space y values
#' @param out Integer (0 or 1), if 1 then return slope, 0 returns real coordinates
#' @param raw Integer (0 or 1), if 1 then return the pixel coordinates
#' @return Data frame of intercept and slope, or coordinates of boundary in either
#'         raw or pixel values
#' @export

extract_coord <- function(boundaryObj, xCor, yCor, out = 1, raw = 0) {
  ### function identifies coordinates and slopes of coords, and returns either of those values for
  ### a boundary object, which is derived from readR function
  if (sum(values(boundaryObj), na.rm = T) == 0) {
    return(data.frame(0, 0)) # return coordinate pair of 0, 0 if there is no boundary in the input raster
  } else {
    boundaryPts <- raster::rasterToPoints(boundaryObj, spatial = T) # pull out xy coords of all points
    if (raw == 1) {
      return(raster::coordinates(boundaryPts))
    } else {
      xExt <- raster::extract(xCor, boundaryPts) # real x value for each point
      yExt <- raster::extract(yCor, boundaryPts) # real y value for each point
      realCoords <- data.frame(xExt+50, yExt)
      realReg <- lm(yExt ~ xExt, data = realCoords) # regress x and y to find slope
      realSlope <- data.frame(coefficients(realReg)[1],
                              coefficients(realReg)[2]) # store in DF
      if (out == 1) {
        return(realSlope) # return the regression values
      } else {
        return(realCoords) # return the real space coords
      }
    }
  }
}
