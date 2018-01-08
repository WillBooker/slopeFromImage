#' Creates mid-pool line
#'
#' Function creates mid-pool line from the intersection between vertical lines
#' and the bed/water surfaces (from \code{\link{find_real}}). This line is the
#' vertical mean of the intersection points.
#' Used to create perpendicular lines and Euclidean distance (depth)
#'
#' @param line1 Spatial Lines object of water surface
#' @param line2 Spatial Lines object of bed surface
#' @return List of intersection points, lines of water and bed surfaces, mid-channel line and sections of mid-channel line
#' @export

create_mid_line <- function(line1, line2) {
  #### Function returns a mid line, from the water surface profile and bed surface profile
  #### adapted from 504 code that uses bars to midchannel, here its profile; thus the legacy description as banks
  #### Works by drawing vertical lines at regular interval, then the mean of the
  #### two points of intersection with the two profiles/banks

  bank1 <- line1
  bank2 <- line2
  sp::proj4string(bank1) <- sp::CRS("+proj=utm") # rgeos needs crs for some stupid reason
  sp::proj4string(bank2) <- sp::CRS("+proj=utm")

  range <- min(c(bank1@bbox[1, 2]), bank2@bbox[1, 2]) -
    max(c(bank1@bbox[1, 1]), bank2@bbox[1, 1]) # maximum lateral difference
  start <- round(max(c(bank1@bbox[1, 1]), bank2@bbox[1, 1]))
  end <- start + range
  line.seq <- seq(start, end, 1) # spacing of lines at 1 pixel
  l <- length(line.seq)
  line.list <- vector("list", length = l)
  i1 <- vector("list", length = l)
  i2 <- vector("list", length = l)

  for (i in 1:l) {
    m <- matrix(c((i-1) * 0.5 + start, (i-1) * 0.5 + start, 0, 20),
                ncol = 2, nrow = 2)
    name <- paste("line", i, sep = ".")
    line.list[[i]] <- assign(name, m)
    line.list[[i]] <- sp::Line(line.list[[i]])
    line.list[[i]] <- sp::SpatialLines(list(Lines(list(line.list[[i]]), ID = "1")))
    proj4string(line.list[[i]]) <- CRS("+proj=utm")
    i1[[i]] <- rgeos::gIntersection(bank1, line.list[[i]]) # intersection between bank/profile and vert lines
    i2[[i]] <- rgeos::gIntersection(bank2, line.list[[i]])
  }

  index <- which.min(c(length(i1), length(i2))) # shorter of two lists
  if (index == 1) {
    bl <- length(i1)
  } else if (index == 2) {
    bl <- length(i2)
  }
  if (bl == 0) {
    return(list(NA, NA, NA, NA, as.data.frame(matrix(NA, ncol = 2)),NA))
  }
  midpoint1 <- vector("list", length = bl)
  midpoint2 <- vector("list", length = bl)
  midpoint <- matrix(nrow = bl, ncol = 2)
  for (j in 1:bl) {
    if (!is.null(i1[[j]])&!is.null(i2[[j]])) {
      midpoint1[[j]] <- i1[[j]]@coords[1] # midpoint x coord from intersection point
      if (mean(bank1@bbox[2, ]) < mean(bank2@bbox[2, ])) { # return mean of y values, dependent on order
        midpoint2[[j]] <- mean(c(max(i1[[j]]@coords[, 2]), min(i2[[j]]@coords[, 2])))
      } else {
        midpoint2[[j]] <- mean(c(max(i2[[j]]@coords[, 2]), min(i1[[j]]@coords[, 2])))
      }
    } else {
      midpoint1[[j]] <- NA
      midpoint2[[j]] <- NA
    }
    midpoint[j, 1] <- midpoint1[[j]]
    midpoint[j, 2] <- midpoint2[[j]]

  }

  result <- list(i1, i2, bank1, bank2, midpoint, line.list)
  return(result)
}
