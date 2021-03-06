% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_coord.R
\name{extract_coord}
\alias{extract_coord}
\title{Return regression value or coordinates for boundary}
\usage{
extract_coord(boundaryObj, xCor, yCor, out = 1, raw = 0)
}
\arguments{
\item{boundaryObj}{Raster of boundary values taken from \code{\link{find_boundary}}}

\item{xCor}{Raster of corrected real space x values}

\item{yCor}{Raster of corrected real space y values}

\item{out}{Integer (0 or 1), if 1 then return slope, 0 returns real coordinates}

\item{raw}{Integer (0 or 1), if 1 then return the pixel coordinates}
}
\value{
Data frame of intercept and slope, or coordinates of boundary in either
        raw or pixel values
}
\description{
Function takes the input boundary raster and returns either the regression values
for the boundary or the pixel/real coordinates of the points.
}
