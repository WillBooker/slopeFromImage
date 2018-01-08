#' Find depth data of pools
#'
#' Function collates the \code{\link{fit_loess}}, \code{\link{find_diff}},
#'  \code{\link{find_real}}, \code{\link{convert_to_poly}},
#' \code{\link{create_mid_line}}, \code{\link{create_perp_line}} and \code{\link{find_dist}}
#' in order to find pool depths for the workingRange images.
#'
#' @param bsl List of bed surface profiles (output from \code{\link{fit_loess}})
#' @param wsl List of water surface profiles (output from \code{\link{fit_loess}})
#' @param k Iteration integer
#' @param xC Raster of corrected real space x values
#' @param yC Raster of corrected real space y values
#' @return Mean, maximum, minimum and standard deviation of pool depths
#' @export


find_depth <- function(bsl, wsl, k, xC, yC) {
  if (sum(is.na(bsl[[1]][[k]]))>0 | sum(is.na(wsl[[1]][[k]])) > 0) {  # if no data in input, skip
    return(list(NA, NA, NA, NA))
  }
  if (any(is.nan(bsl[[2]][[k]]$residuals))) {
    return(list(NA, NA, NA, NA))
  }
  step1 <- find_diff(bsl, wsl, k, out = 1) # produce polygon
  if (step1 == "empty" | is.na(step1)) {
    return(list(NA, NA, NA, NA))
  } else if (step1 == "nodepth") {
    return(list(0, 0, 0, 0))
  }
  n <- length(step1)
  correctRList <- vector("list", length = n)  # store real coordinates of polygon
  for (j in 1:n) {
    realCoords <- find_real(step1[[j]], xCor, yCor)
    correctRList[[j]] <- realCoords
  }
  if (sum(is.na(correctRList))>0) {
    return(list(NA, NA, NA, NA))
  }
  polyOut <- convert_to_poly(correctRList)  # create realcoord polygon
  polyOutB <- rgeos::gBuffer(polyOut, width = 0)  # solve rgeos issue
  step2 <- find_diff(bsl, wsl, k, out = 0)  # create lines
  wsC <- find_real(step2[[1]]@lines[[1]]@Lines[[1]]@coords, xCor, yCor)  # real coords of lines
  bsC <- find_real(step2[[2]]@lines[[1]]@Lines[[1]]@coords, xCor, yCor)
  wscL <- sp::SpatialLines(list(Lines(list(Line(wsC)), ID = "1")))
  bscL <- sp::SpatialLines(list(Lines(list(Line(bsC)), ID = "1")))

  wscI <- raster::intersect(wscL, polyOutB)  # only lines overlapped by polygon
  bscI <- raster::intersect(bscL, polyOutB)
  sp::proj4string(wscI) <- sp::CRS("+proj=utm")
  sp::proj4string(bscI) <- sp::CRS("+proj=utm")

  midL <- create_mid_line(wscI, bscI)  # mid line of profiles
  perpL <- create_perp_line(midL, wscI, bscI)  # perp lines of midL
  outD <- find_dist(perpL[[2]])  # mean, max, min, sd distance
  return(outD)
}
