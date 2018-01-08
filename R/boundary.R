#' Create raster of boundary
#'
#' Function returns a raster object identifying the location of the boundary
#' between two given classes, for a given classified raster.
#'
#' @param ras Input classified raster
#' @param class1 Vector of class value above the boundary
#' @param class2 Vector of class value below the boundary
#' @param xMin Minimum x extent of raster (pixel number)
#' @param yMin Minimum y extent of raster (pixel number)
#' @param xMax Maximum x extent of raster (pixel number)
#' @param yMax Maximum y extent of raster (pixel number)
#' @return Raster object containing cells (value = 1) of boundary location
#' @export

find_boundary <-  function(ras, class1, class2,
                           xMin = 64, xMax = 2011, yMin = 275, yMax = 651) {

  rown <- raster::nrow(ras)
  coln <- raster::ncol(ras)

  l1 <- length(class1)
  l2 <- length(class2)
  l <- l1+l2
  class <- append(class1, class2)
  nameVec <- vector(length = l)
  rasList <- vector("list", length = l)
  overList <- vector("list", length = 2)
  for (i in 1:l) {
    nameVec[i] <- paste0("rc", i)
    rasList[[i]] <- assign(nameVec[i], ras)
    if (i <= l1) {
      rasList[[i]][rown,] <- NA
    } else {
      rasList[[i]][1,] <- NA
    }
    rasList[[i]][rasList[[i]] != class[i]] <- NA
    rasList[[i]][!is.na(rasList[[i]])] <- 1
  }

  if (l1 == 1) {
    overList[[1]] <- rasList[[1]]
  } else {
    overList[[1]] <- raster::overlay(rasList[[1]], rasList[[2]], fun = function(x, y) {
      ifelse(x == 1 | y == 1, 1, 0)})
  }
  if (l2 == 1) {
    overList[[2]] <- rasList[[l]]
  } else {
    overList[[2]] <- raster::overlay(rasList[[l-1]], rasList[[l]], fun = function(x, y) {
      ifelse(x == 1 | y == 1, 1, 0)})
  }
  rcf <- ras
  rcf[rcf != 1] <- NA
  xext <- max(raster::rasterToPoints(rcf)[, 1]) - 64
  if (is.infinite(xext)) {
    xext <- 1
  }
  nrow(overList[[1]]) <- rown
  nrow(overList[[2]]) <- rown
  overM1 <- raster::as.matrix(overList[[1]])
  overM2 <- raster::as.matrix(overList[[2]])
  overM1[, xext:coln] <- NA
  overM2[, xext:coln] <- NA
  boundMat <- overM1[1:(rown - 1),] == overM2[2:rown,]

  if (sum(boundMat, na.rm = T) > 0) {
    # return raster which only includes the highest y coord
    boundRas <- raster::raster(boundMat,
                       xmn = xMin, xmx = xMax, ymn = yMin, ymx = yMax-1)
    boundXY <- raster::xyFromCell(boundRas, raster::which.max(boundRas))
    if (NROW(boundXY) == 1) {
      boundP <- sp::SpatialPoints(boundXY)
    } else {
      boundXY <- data.frame(
        boundXY[order(boundXY[, 1], -boundXY[, 2], decreasing = F),])
      dup <- duplicated(boundXY[, 1])
      dup <- as.logical(1- dup)
      boundXYcor <- boundXY[dup,]
      boundP <- sp::SpatialPoints(boundXYcor)
    }
    boundOut <- raster::rasterize(boundP, boundRas, field = 1)
  } else {
    # return empty raster
    boundOut <- raster::raster(nrows = rown, ncol = coln,
                       xmn = xMin, xmx = xMax, ymn = yMin, ymx = yMax)
  }
  return(boundOut)
}
