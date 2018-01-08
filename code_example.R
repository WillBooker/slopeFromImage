######Master list of all code necessary to run from raw image to slope values
# leads to a redo
library("EBImage")
library("raster")
library("randomForest")
library("dplyr")
library("slopeFromImage")

### Boundary Types
boundary_type <- "BS" ### WS = water surface, BS = bed surface
if (boundary_type == "WS") {
  class1In <- 4
  class2In <- c(1, 2)
} else if (boundary_type == "BS") {
  class1In <- c(2, 4)
  class2In <- 1
}

### Goal of running script
functionCall <- "write" ### write creates rasters from raw images and saves as .txt (.csv),
### read imports those files to convert into boundaries
output <- "rp" ### r = regression, p = points, rp = both
loessing <- "y" ### y = yes, n = no

path <- "C:/Users/worker/Documents/research"
folderName <- "run2" # name of folder desired to be analysed

fitMod <- readRDS("C:/Users/worker/Documents/research/RFrun17model.rds")  # necessary for classification regression
xCorrection <- read.csv("C:/Users/worker/Documents/research/xCorrection.txt", header = F)
yCorrection <- read.csv("C:/Users/worker/Documents/research/yCorrection.txt", header = F)

xCor <- as.matrix(xCorrection, nrow = 376) # convert to matrix
yCor <- as.matrix(yCorrection, nrow = 376)

### extent values for the rasters/images
xmin1 <- 64
xmax1 <- 2011
ymin1 <- 275
ymax1 <- 651
xMin <- xmin1
xMax <- xmax1
yMin <- ymin1
yMax <- ymax1

### convert to rasters with correct extent
xCor <- raster(xCor)
extent(xCor) <- extent(xmin1, xmax1, ymin1, ymax1)
yCor <- raster(yCor)
extent(yCor) <- extent(xmin1, xmax1, ymin1, ymax1)



{
  ### vectors of image identities, odd is included, even if ignored
  run2V <- c(15, 2, 15, 2, 15, 3, 13, 2, 15, 1, 15, 2, 16, 2, 15, 3, 15, 2, 15, 2,
             15, 3, 15, 1, 15, 3, 14, 2, 15, 3, 14, 2, 15, 2, 15, 2, 15, 2, 15, 2,
             15, 2, 15, 2, 15, 1, 15, 4, 15, 2, 14, 3, 14, 3, 15, 2, 15, 3, 15, 2,
             16, 2, 15, 2, 15, 2, 15, 1, 14, 3, 15, 3, 14)
  run3V <- c(16, 3, 14, 3, 14, 3, 15, 2, 14, 3, 15, 2, 15, 4, 13,
             3, 15, 2, 14, 2, 15, 2, 15, 6, 14, 4, 15, 2, 15, 3,
             15, 2, 14, 2, 14, 2, 15, 2, 15, 2, 15, 2, 14, 3, 14, 2)
  run4V <- c(15, 2, 14, 3, 14, 2, 15, 2, 15, 2, 15, 3, 15, 2, 15, 3, 15, 2, 15, 2, 15, 5,
             15, 2, 15, 2, 15, 2, 15, 3, 16, 2, 15, 2, 15, 4, 14, 3, 14, 6)
  run5V <- c(15, 3, 15, 3, 15, 4, 15, 3, 15, 3, 14, 4, 15, 3, 15, 4,
             16, 3, 15, 4, 15, 3, 15, 4, 15, 3, 15, 8, 15, 3, 15, 3,
             15, 3, 15, 4, 14, 7, 15, 3, 15, 2, 15, 4, 15, 3, 15, 4,
             15, 3, 15, 4, 15, 4, 15, 3, 15, 4, 15, 3, 15, 3, 15, 4,
             15, 3, 15, 4, 15, 4, 15)
  run6V <- c(15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 2, 15,
             4, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 5, 9, 3)
  run7V <- c(15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 3, 15, 2, 15, 5, 15, 2, 15,
             3, 15, 3, 15, 3, 16, 2, 15, 3, 15, 3, 15, 3, 15, 3, 15, 4, 14, 4,
             14, 5, 15, 4, 15, 4, 14, 4, 14, 4, 15, 4, 15, 4, 14, 5, 14, 4, 14,
             3)
  run8V <- c(15, 3, 15, 3, 15, 4, 15, 3, 15, 5, 15, 4, 15, 3, 15, 3, 15, 3, 15,
             6, 14, 3, 15, 4, 14, 3, 15, 4, 15, 4, 14, 6, 15, 3, 15, 5, 15, 3,
             15, 12, 14, 4, 15, 3, 15, 3, 15, 5, 14, 1)
  run9V <- c(15, 3, 15, 3, 15, 3, 15, 3, 15, 4, 15, 4, 15, 4, 15, 3, 15, 4, 15,
             4, 15, 4, 15, 4, 17, 4, 15, 36, 15, 3, 15, 5, 15, 4, 15, 4,
             14, 2)
  run10V <- c(15, 3, 15, 4, 14, 4, 15, 3, 15, 4, 14, 4, 14, 4, 15, 4, 15,
              5, 14, 4, 15, 4, 15, 4, 15, 5, 14, 8, 13, 4, 15, 4, 14, 5,
              15, 4, 14, 6, 14, 4)
  run11V <- c(15, 3, 15, 4, 15, 7, 14, 4, 15, 4, 14, 4, 15, 7, 14, 7, 15, 4,
              15, 4, 14, 4, 15, 4, 15, 4, 16, 5, 15, 4, 14, 5, 14, 4, 15, 4,
              15, 4, 15, 43, 15, 3, 15, 4, 15, 3, 16, 3, 14, 4, 15, 3, 15, 6,
              15, 4, 15, 4, 15, 3, 15, 4, 15, 3, 15, 4, 15, 3, 15, 4, 15, 4)
  run12V <- c(15, 3, 14, 7, 14, 4, 15, 4, 14, 5, 14, 47, 15, 3, 15, 4, 15, 3,
              15, 4, 15, 6, 15, 4, 15, 3, 16, 4, 15, 4, 15, 3, 15, 4, 15, 3, 15,
              5, 15, 6, 15, 5, 15, 3, 15, 4, 15, 3, 15, 7, 15, 4, 15, 3, 15, 4,
              15, 3, 15, 7, 15, 4, 15, 4, 15, 5, 15, 4, 15, 3, 15, 1)
  run13V <- c(15, 3, 16, 4, 15, 3, 15, 3, 15, 4, 15, 3, 15, 4, 15, 4, 15, 3, 15,
              4, 15, 4, 15, 3, 3, 29, 12, 3, 15, 4, 15, 3, 15, 3, 15, 4, 18, 4,
              15, 4, 15, 3, 15, 4, 15, 5, 15, 4, 15, 3, 15, 1)
  run14V <- c(15, 3, 15, 3, 15, 4, 15, 4, 15, 4, 15, 3, 15, 5, 15, 3, 15, 6, 15, 4,
              15, 3, 10, 4, 5, 3, 15, 4, 15, 3, 15, 5, 15, 4, 15, 3, 15, 4, 15, 4, 15, 5)
  run15V <- c(15, 4, 15, 3, 15, 3, 15, 5, 15, 3, 15, 4, 15, 4, 15, 3, 15, 4, 15, 4, 15, 7,
              15, 4, 15, 3, 15, 4, 15, 4, 15, 4, 15, 3, 15, 3, 15, 6, 15, 4, 15, 3)
  run17V <- c(15, 3, 15, 3, 15, 3, 15, 3, 15, 4, 15, 3, 15, 5, 15, 4, 15, 3, 15, 4,
              15, 3, 15, 58, 7, 4, 15, 3, 15, 3, 15, 3, 15, 3, 15, 4, 15, 3, 15, 4,
              15, 3, 15, 3, 15, 3, 15, 4, 15, 3, 15, 4, 15, 3, 16, 4, 15, 4, 15, 6,
              15, 3, 15, 3, 15, 3, 15, 2)}

##### Section for read/write .txt files

setwd(path) # set to directory containing folders of relevant runs
setwd(folderName) # set for the run
imageVec <- paste0(folderName, "V") # pull out the vector of images
if (functionCall == "write") {
  classified_raster_list <- write_txt(folderName, imageVec, fitMod) # write .txts
} else if (functionCall == "read") {
  maskObj <- read.csv(paste0(folderName, "mask.txt"), header = F) # read mask obj in to remove roughness elements
  digMaskPoly <- SpatialPolygons(list(Polygons(list(Polygon(maskObj)), 1)))
  fileVec <- list.files(pattern = "pred.txt")
  len <- length(fileVec)
  boundList <- vector("list", length = len)
  message(paste("There are", len, "iterations to go through"))
  for (i in 1:len) {
    boundList[[i]] <- read_txt(fileVec[i], class1In, class2In) # run boundary identifier for all pred.txt files
    if (i%%5 == 0) {
      message(paste("Current iteration:", i, "of:", len, sep = " ")) # for every 5 iterations print the number
    }
    message("boundList (readR) completed") # for each boundary identified
  }

  leng <- length(boundList)
  pointsList <- vector("list", length = leng)
  regList1 <- vector("list", length = leng)
  if (grepl("r", output)) {
    for (i in 1:leng) {
      if(!is.null(boundList[[i]])) {
        regList1[[i]] <- extract_coord(boundList[[i]], xCor, yCor, join = 1) # reg slope and coeff for each image
        names(regList1[[i]]) <- c("x","y")
        if (i%%5 == 0)
          message(paste("Current iteration:", i, "of:", leng, sep = " ")) # for every 5 iterations print the number
      }
    }
    message("regList (coordExtractR) completed")
  }
  if (grepl("p", output)) {
    for (i in 1:leng) {
      if(!is.null(boundList[[i]])) {
        pointsList[[i]] <- extract_coord(boundList[[i]], # pixel coordinates from which above is derived
                                         xC, yC, out = 2, join = 1, raw = 1)
        pointsList[[i]] <- cbind(pointsList[[i]], i)  # store iteration value to make deseg easier when reading file
        names(pointsList[[i]]) <- c("x","y","i")
        if (i%%5 == 0)
          message(paste("Current iteration:", i, "of:", leng, sep = " ")) # for every 5 iterations print the number
      }
    }
    message("pointsList (coordExtractR) completed")
  }
  regVec <- do.call(rbind, lapply(regList1, function(x) unlist(x)))

  setwd(path)
  setwd(folderName)
  reg_file_name <- paste0("regList", boundary_type, ".txt")
  if (grepl("r", output)) {
    write.table(as.data.frame(do.call(rbind, lapply(regList1, function(x) x[2]))), #save list of regression values to .txt file
                reg_file_name, sep = ",", row.names = FALSE, col.names = F)
  }
  p_file_name <- paste0("pointsList", boundary_type, ".txt")
  if (grepl("p", output)) {
    write.table(as.data.frame(do.call(rbind, lapply(pointsList, function(x) x))), #save list of pixel coordinates to .txt file
                p_file_name, sep = ",", row.names = FALSE, col.names = F)
  }
  plot(1, -regList1[[1]][2], xlim = c(0, leng), ylim = c(0, 0.25), col = "black", cex = 0.6, pch = 3)
  for (i in 2:leng) {points(i,-regList1[[i]][2], col = "black", cex = 0.6, pch = 3)}
  lines(1:leng, abs(regVec[, 2]), col = "black")
}
#####

##### Pool depth section
if (loessing == "y") {
  library("rgeos")
  wdPointsDF <- read.csv("pointsListWS.txt", header = F)  # read water surface points
  bdPointsDF <- read.csv("pointsListBS.txt", header = F)  # read bed surface points

  names(bdPointsDF) <- c("x", "y", "i")
  names(wdPointsDF) <- c("x", "y", "i")

  bd <- fit_loess(bdPointsDF)
  ws <- fit_loess(wdPointsDF)
  txtLen = length(bd[[1]])
  totalList <- vector("list", length = txtLen)
  for (i in 1:txtLen) {
    totalList[[i]] <- find_depth(bd, ws, i, xCor, yCor)
    if (i%%5 == 0){
      message(paste("Current iteration:", i, "of:", txtLen, sep = " ")) # for every 5 iterations print the number
    }
  }
  saveRDS(totalList, file = "poolstat.rds")

  assign(paste0(folderName, "mean"), sapply(totalList, function(x) x[[1]]))
  assign(paste0(folderName, "max"), sapply(totalList, function(x) x[[2]]))
  assign(paste0(folderName, "min"), sapply(totalList, function(x) x[[3]]))
  assign(paste0(folderName, "sd"), sapply(totalList, function(x) x[[4]]))
}
