#' Creates Polygons from Coordinates
#'
#' Function converts data.frame output \code{\link{find_real}} to SpatialPolygons
#' and saves as SpatialPolygonsDataFrame
#'
#' @param poly_out Data frame of output from \code{\link{find_real}}
#' @return SpatialPolygonsDataFrame of pools
#' @export

convert_to_poly <- function(poly_out) {
  ### Converts points to polygon
  xcSP <- vector("list", length = length(poly_out))

  for (i in 1:length(poly_out)) {
    if (sum(is.na(poly_out[[i]])) == 0) {
      xcSP[[i]] <- sp::SpatialPolygons(list(Polygons(list(Polygon(poly_out[[i]][, 1:2])),
                                                 ID = i)))
    } else {
      xcSP[[i]] <- NA
    }
  }
  xcSP <- xcSP[!is.na(xcSP)]
  joined <- sp::SpatialPolygons(lapply(xcSP, function(x) {x@polygons[[1]]}))
  joinSPDF <- sp::SpatialPolygonsDataFrame(Sr = joined,
                                    data = data.frame(i = 1:length(xcSP)),FALSE)
  return(joinSPDF)
}
