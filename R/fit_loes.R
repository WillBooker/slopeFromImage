#' Creates loess object of surface boundaries
#'
#' Function takes a data.frame of boundary coordinates and returns list of loess
#' objects and the corrected coordinates used to fit those lines. ClearList
#' removes excessively erroneous values, based on stepped gradient.
#'
#' @param pointsListDF Saved output from \code{\link{extract_coord}}; data.frame of points of surface
#' @return List of clearList (list of x and y values used for loessList) and loessList (list of loess objects)
#' @export

fit_loess <- function(pointsListDF) {
  #### Function derives a loess, span 0.25, of the given points DF from above
  #### Returns the points as DF of the input, and the loess object


  splitList <- split(pointsListDF, f = pointsListDF$i) # convert DF to list, seperated by iteration (i)
  n <- length(levels(as.factor(pointsListDF[, 3])))

  clearList <- vector("list", length = n)
  loessList <- vector("list", length = n)
  for (i in 1:n) {
    if (length(splitList[[i]][,1]) > 3) {
      splitList[[i]] <- splitList[[i]][order(splitList[[i]][, 1]),] # reorder as x increases
      xdifference <- diff(splitList[[i]][, 1]) # sequential differences for x values
      ydifference <- diff(splitList[[i]][, 2]) # sequential differences for y values
      xd <- abs(xdifference)<100 # only keep xdifferences less than 100 pixels
      yd <- abs(ydifference)<15 # only keep ydifferences less than 15 pixels
      xdifference2 <- xdifference[xd]
      ydifference2 <- ydifference[yd]
      len <- length(xdifference)
      remVec <- rep(0,(len + 1))
      remVec[2:(len+1)] <- xd * yd
      clearList[[i]] <- splitList[[i]][(which(remVec == 1)),1:2] # return only wanted values for loess calc
      if ((range(splitList[[i]][, 2])[2] - range(splitList[[i]][, 2])[1]) > 4) { # need range of y values to be greater than 4 otherwise NA
        xl <- loess(y ~ x, data = clearList[[i]],
                    control = loess.control(surface = "interpolate"), span = 0.25)
        loessList[[i]] <- xl
      } else {
        clearList[[i]] <- NA
        loessList[[i]] <- NA
      }
    } else {
      clearList[[i]] <- NA
      loessList[[i]] <- NA
    }
  }
  return(list(clearList, loessList))
}
