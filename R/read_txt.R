#' Reads image .txts and finds boundary
#'
#' Function reads in either all pred.txt files or single files at a time and
#' runs \code{\link{find_boundary}} to create a raster of boundary values for
#' the two given classes.
#'
#' @param input If single file method this is the individual file name
#' @param class1 Vector of class value above the boundary
#' @param class2 Vector of class value below the boundary
#' @param folder Character string ("y"/"n") as to whole folder or single files respectively
#' @param xMin Minimum x extent of raster (pixel number)
#' @param yMin Minimum y extent of raster (pixel number)
#' @param xMax Maximum x extent of raster (pixel number)
#' @param yMax Maximum y extent of raster (pixel number)
#' @return Raster(s) of boundary
#' @export

read_txt <- function(input, class1, class2, folder = "n",
                     xMin = 64, xMax = 2011, yMin = 275, yMax = 651) {
  if (folder == "y") {
    fileVec <- list.files(pattern = "pred.txt") #  list all predictive matrix files in directory
    len <- length(fileVec)
    predList <- vector("list", length = len)
    for (i in 1:len) {
      input <- fileVec[i]
      tab <- read.csv(input)#  read file
      pred <- raster::raster(as.matrix(tab), xmn = xMin, xmx = xMax, ymn = yMin, ymx = yMax) # generate raster from file
      pred[pred == 3] <- 2
      pred[pred == 6] <- 2
      pred[pred == 7] <- 4
      predClumpPS <- raster::mask(pred, digMaskPoly, inverse = T)
      predClumpOG <- remove_patch(predClumpPS)
      predClump <- raster::focal(predClumpOG, w = matrix(1, 7, 7), find_common)
      if (sum(values(predClump) == 1, na.rm = T) > 10) {
        predBound <- find_boundary(predClump, class1, class2)
      } else {
        predBound <- raster::raster(xmn = xMin, xmx = xMax, ymn = yMin, ymx = yMax)
        predBound[] <- NA
      }
      predList[[i]] <- predBound # store into list
      if (i%%5 == 0) {
        message(paste("Current iteration:", i, "of:", len, sep = " ")) #  for every 5 iterations print the number
      }
    }
    return(predList)
  } else if (folder == "n") {
    tab <- read.csv(input)#  read file
    pred <- raster::raster(as.matrix(tab), xmn = xMin, xmx = xMax, ymn = yMin, ymx = yMax) # generate raster from file
    pred[pred == 3] <- 2
    pred[pred == 6] <- 2
    pred[pred == 7] <- 4
    predClumpPS <- raster::mask(pred, digMaskPoly, inverse = T)
    predClumpOG <- remove_patch(predClumpPS)
    predClump <- raster::focal(predClumpOG, w = matrix(1, 7, 7), find_common)
    if (sum(values(predClump) == 1, na.rm = T) > 10) {
      predBound <- find_boundary(predClump, class1, class2)
    } else {
      predBound <- raster::raster(xmn = xMin, xmx = xMax, ymn = yMin, ymx = yMax)
      predBound[] <- NA
    }
    return(predBound)
  }
}
