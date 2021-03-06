% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_diff.R
\name{find_diff}
\alias{find_diff}
\title{Creates polygon or surface lines}
\usage{
find_diff(bsLoess, wsLoess, i, out)
}
\arguments{
\item{bsLoess}{List of bed surface loess objects output from \code{\link{fit_loess}}}

\item{wsLoess}{List of water surface loess objects output from \code{\link{fit_loess}}}

\item{i}{Iteration integer}

\item{out}{Integer (0 or 1) determining output type. 1 is polygon, 0 is spatial lines}
}
\value{
Polygon of difference or lines of water/bed surface
}
\description{
Function creates a polygon of the difference between water and bed surfaces,
or returns them as lines objects.
}
