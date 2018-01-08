#' Find pool depths
#'
#' Function calculates euclidian distance for all perpendicular lines between
#' water and bed surface, and the statistics from that.
#'
#' @param channel SpatialLines object from the output of \code{\link{create_perp_line}}
#' @return List of mean, maximum, minimum and standard deviation of distance
#'         between water surface and bed surface profiles, equivalent to the pool depth.
#' @export

find_dist <- function(channel) {
  #### Function returns metrics (mean, max, min, sd) of water depths (aka surface to bed distance)

  if (sum(is.na(channel))>0) {
    return(list(NA, NA, NA, NA))
  }
  l <- length(channel)
  coor <- sapply(channel, function(x) {
    bb <- slot(x, "bbox")
    dx <- bb[3] - bb[1]
    dy <- bb[4] - bb[2]
    dist <- sqrt((dx ^ 2) + (dy ^ 2))  # euclid distance
  })
  distVec <- coor[coor != 0]  # remove 0s from DF, i.e. non pools
  return(list(mean(distVec), max(distVec), min(distVec), sd(distVec)))
}
