#' Find most common factor
#'
#' Function returns most frequent value for a given input
#'
#' @param x Object that can be coerced into vector
#' @return Value of most frequently occurring value
#' @export

find_common <- function(x) {
  ux <- unique(x)
  ux <- ux[!is.na(ux)]
  return(ux[which.max(tabulate(match((x), ux)))])
}
