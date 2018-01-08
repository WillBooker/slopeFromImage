#' Convert image to classified raster
#'
#' Function loads an image, extracts colour bands and smoothes into raster. This
#' raster is then classified according to a prebuilt randomForests linear model.
#' This raster is then reclassified into four umbrella classes and saved as a
#' .csv file with the suffix 'pred.txt'.
#'
#' @import randomForest
#' @param fileName Given file name of image
#' @param mod randomForests model
#' @param xmin1 Minimum x extent of raster (pixel number)
#' @param ymin1 Minimum y extent of raster (pixel number)
#' @param xmax1 Maximum x extent of raster (pixel number)
#' @param ymax1 Maximum y extent of raster (pixel number)
#' @param d Size of smoothing window
#' @return Saved file of classified raster
#' @export

image_rasterise <- function(fileName, mod,
                            xmin1 = 64, xmax1 = 2011, ymin1 = 275, ymax1 = 601,
                            d = 3) {
  ## Convert image file into a raster stack of its component three bands, whilst transposing (necessary)
  ## and smoothing (optional, but default = 9) the data. Classifies image according to model created in
  ## regressR file.

  img <- EBImage::readImage(fileName) # read image
  imgSub <- img[xmin1:(xmax1-1), ymin1:(ymax1-1),] # use calibrated coordinates according to calibR
  rBand <- imgSub[, , 1] # red values
  gBand <- imgSub[, , 2] # green values
  bBand <- imgSub[, , 3] # blue values

  redRas <- raster::raster(EBImage::imageData(rBand),
                   xmn = ymin1, ymn = xmin1, xmx = ymax1, ymx = xmax1)# raster same extent as calibR
  redRas <- raster::t(redRas) * 255 # transpose and convert to 8bit
  redRas <- raster::focal(redRas, w = matrix(1, d, d), mean) # smooth
  greenRas <- raster::raster(EBImage::imageData(gBand),
                     xmn = ymin1, ymn = xmin1, xmx = ymax1, ymx = xmax1)
  greenRas <- raster::t(greenRas) * 255
  greenRas <- raster::focal(greenRas, w = matrix(1, d, d), mean)
  blueRas <- raster::raster(EBImage::imageData(bBand),
                    xmn = ymin1, ymn = xmin1, xmx = ymax1, ymx = xmax1)
  blueRas <- raster::t(blueRas) * 255
  blueRas <- raster::focal(blueRas, w = matrix(1, d, d), mean)
  imgRas <- raster::stack(redRas, greenRas, blueRas)
  names(imgRas) <- c("red", "green", "blue")

  pred <- raster::predict(imgRas, mod, type = 'response') # predict raster from model, according to colour values
  ext <- raster::extent(xmin1, xmax1, ymin1, 651) # add blank space to match up to dim of calibration image
  pred <- raster::extend(pred, ext)
  pred[pred == 3] <- 2 # sediment water to water
  pred[pred == 6] <- 2 # water surface to water
  pred[pred == 7] <- 4 # shadow to background

  modPredMat <- matrix(raster::getValues(pred), nrow = dim(redRas)[1], ncol = dim(redRas)[2]) # convert to matrix to save
  modPredMat[is.na(modPredMat)] <- 0 # replace NAs w/ 0s due to me not being able to figure out NAs in py
  n <- nchar(fileName) # length of file name
  fileNameSub <- substr(fileName, 1, n-4) # replace .txt w/ pred.txt in order to differentiate files
  namePred <- paste(fileNameSub, "pred.txt", sep = "")
  write.table(modPredMat, namePred, sep = ",", row.names = T, col.names = T) # saves matrix of classes
}
