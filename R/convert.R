
#' Convert x3p header information to microns from meters
#'
#' ISO standard 5436_2 asks for specification of values in meters.
#' For topographic surfaces collected by microscopes values in microns are 
#' more readable. Besides scaling the values in the surface matrix, corresponding 
#' increments are changed to microns as well. 
#' @param x3p x3p file with header information in meters
#' @return x3p with header information in microns
#' @export
#' @import assertthat
x3p_m_to_mum <- function(x3p) {
  assert_that("x3p" %in% class(x3p))
  
  # make sure all measurements are in microns
  x3p$surface.matrix <- x3p$surface.matrix * 10^6
  x3p$header.info$incrementY <- x3p$header.info$incrementY * 10^6
  x3p$header.info$incrementX <- x3p$header.info$incrementX * 10^6
  x3p
}