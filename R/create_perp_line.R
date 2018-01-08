#' Create perpendicular lines
#'
#' Functions creates lines perpendicular to the mid-pool line (from \code{\link{create_mid_line}}).
#' [[1]] is full perp line, [[2]] is between the mid-pool line and the bed/water surfaces.
#'
#' @param midLine Output from \code{\link{create_mid_line}}
#' @param b1 SpatialLines object equivalent to water surface
#' @param b2 SpatialLines object equivalent to bed surface
#' @return List of full SpatialLines perpendicular lines and the extent that
#'         intersect with surfaces.
#' @export

create_perp_line <- function(midLine, b1, b2) {
  #### Function generates lines extending from midchannelline to banks (profile)
  #### in order to calculate the mean water depth

  xy <- data.frame(midLine[[5]]) # dataframe the midpoints
  xy <- xy[complete.cases(xy),] # remove NAs i.e. non-polygon intersecting values
  l <- length(xy[, 1]) - 1
  if (l < 1) {
    return(list(NA, NA))
  }
  for (i in 1:l) {
    xy[i, 3] <- (xy[i, 2] - xy[i + 1, 2])/1
  }
  degrees <- atan(xy[, 3]) # convert m to degrees
  t1o <- sin(degrees) # convert above to x coords
  t1a <- cos(degrees) # convert above to y coords
  newxp <- xy[, 1] + t1o # xpositive
  newxn <- xy[, 1] - t1o # xnegative
  newyp <- xy[, 2] + t1a # ypositive
  newyn <- xy[, 2] - t1a # ynegative
  perp <- vector("list", length = l)
  channel <- vector("list", length = l)
  for (i in 1:l) {
    p <- c(newxp[i], newxn[i], newyp[i], newyn[i]) # matrix of points
    p <- matrix(p, nrow = 2, ncol = 2)
    perp[[i]] <- p
    perp[[i]] <- sp::Line(perp[[i]])
    perp[[i]] <- sp::SpatialLines(list(Lines(list(perp[[i]]), ID = "1")))
    sp::proj4string(perp[[i]]) <- sp::CRS("+proj=utm")
    p.intersect1 <- rgeos::gIntersection(perp[[i]], b1) # intersection with banks
    p.intersect2 <- rgeos::gIntersection(perp[[i]], b2)
    if (is.null(p.intersect1)|is.null(p.intersect2) == "TRUE") { # if no intersection, just use full width honk honk
      channel[[i]] <- perp[[i]]
    } else {
      b1.p <- which.max(p.intersect1@coords[, 2])
      b2.p <- which.min(p.intersect2@coords[, 2]) # order line storage
      c <- matrix(c(p.intersect1@coords[b1.p, 1], p.intersect2@coords[b2.p, 1],
                    max(p.intersect1@coords[b1.p, 2]), p.intersect2@coords[b2.p, 2]),
                  nrow = 2, ncol = 2)
      channel[[i]] <- c
      channel[[i]] <- sp::Line(channel[[i]])
      channel[[i]] <- sp::SpatialLines(list(Lines(list(channel[[i]]), ID = "1")))
      sp::proj4string(channel[[i]]) <- sp::CRS("+proj=utm")
    }
  }
  results <- list(perp, channel)
  return(results)
}
