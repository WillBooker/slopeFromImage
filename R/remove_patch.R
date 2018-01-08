#' Creates raster and removes small clumps
#'
#' Function takes a classified raster (see \code{\link{image_rasterise}}) and removes
#' clumps smaller than n pixels from it. Done for the removal of erroneously
#' identified patches.
#'
#' @param n Number of pixels clumps to be removed should be smaller than
#' @param ras Input classified raster
#' @return Raster with clumps removed
#' @export

remove_patch <- function(ras, clump_size = 15) {
  ### clumpeR reduces the number of patches, in order to reduce the number of false positives
  ### incurred by incorrect identification due to RF regression model

  r <- raster::extend(ras, c(1, 1)) # add extra row and column
  clVal <- unique(raster::values(r)) # get values of cells
  clVal <- clVal[!clVal == 0] # remove = 0
  clVal <- clVal[!is.na(clVal)]
  r.NA <- raster::setValues(raster(r),1) # raster of size of r
  r.NA[r == 0] <- NA

  ## Taken from stackOverflow
  ##


  for (i in clVal) {
    # create & fill in class raster
    r.class <- raster::setValues(raster(r), NA)
    r.class[r == i]<- 1
    # clump class raster
    clp <- raster::clump(r.class, directions = 4)
    # calculate frequency of each clump/patch
    cl.freq <- as.data.frame(freq(clp))
    # store clump IDs with frequency 1
    rmID <- cl.freq$value[which(cl.freq$count < clump_size)]
    # assign NA to all clumps whose ID"s have frequency 1
    r.NA[raster::match(clp, rmID)] <- NA
  }

  r <- r * r.NA

  r <- raster::crop(r, raster::extent(ras)[1:4])

  return(r)
}
