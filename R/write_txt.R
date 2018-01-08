#' Write all .tif files to classified .txt files
#'
#' Function applies the \code{\link{image_rasterise}} to all .tif files in a given
#' folder.
#'
#' @param folderName Name of folder to run function in
#' @param vRange Vector of values from which the working range and ignored photos
#'               can be calculated. Odd values are included, even are excluded.
#' @return List of classified rasters and also save files to folder
#' @export

write_txt <- function(folderName, vRange, mod) {
  ## Identifies all files in a folder, reads them in and saves them individually as matrices
  ## workingRange only includes non scan images

  fileVec <- list.files(pattern = "\\.tif$") # list all .tif files in directory
  vRange = get(vRange)
  l <- length(vRange) # number of files
  vecFrom <- vRange[-l]
  vecFrom <- append(vRange, 1, after = 0)
  vecTo <- vRange
  vfCS <- cumsum(vecFrom) # indices of from values
  vtCS <- cumsum(vecTo) # indices of to values
  rList <- vector("list", length = l)
  for (i in 1:l) {
    rList[[i]] <- c(vfCS[i], vtCS[i]) # create list of paired a to b indices
  }
  workingRange <- vector()
  for (i in 1:l) {
    if (i%%2 != 0) {
      workingRange <- append(workingRange, fileVec[rList[[i]][1]:rList[[i]][2]]) # name of non-scan files
    }
  }
  len <- length(workingRange)
  message(paste(c("There are ", len, " file(s) in ", path, "/", folderName)),
          collapse = "")
  if (len == 0) {
    stop(paste0(c("There are no files in ", path, "/", folderName))) # if the folder is empty return this
  } else {
    rasterList <- vector("list", length = len)
    for (i in 1:len) {
      message(i)
      pred <- slopeFromImage::image_rasterise(workingRange[i], mod)
      message(i)
      rasterList[[i]] <- pred
      if (i%%5 == 0)
        message(paste("Current iteration:", i, "of:", len, sep = " ")) # for every 5 iterations print the number
    }
  }
  return(rasterList)
}
