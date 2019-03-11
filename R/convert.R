
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
  x3p_scale_unit(x3p = x3p, scale_by=10^6)
}


#' Scale x3p object by given unit
#' 
#' x3p objects can be presented in different units. ISO standard 5436_2 asks for specification of values in meters.
#' For topographic surfaces collected by microscopes values in microns are 
#' more readable. This functions allows to convert between different units. 
#' @param x3p object in x3p format, 3d topographic surface.
#' @param scale_by numeric value. Value the surface to be scaled by. While not enforced, values of `scale_by` make most sense as multiples of 10 (for a metric system).
#' @return x3p with header information in microns
#' @export
#' @examples 
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' logo # measurements in meters
#' x3p_scale_unit(logo, scale_by=10^6) # measurements in microns
x3p_scale_unit <- function(x3p, scale_by) {
  assert_that("x3p" %in% class(x3p))
  
  # make sure all measurements are in microns
  x3p$surface.matrix <- x3p$surface.matrix * scale_by
  x3p$header.info$incrementY <- x3p$header.info$incrementY * scale_by
  x3p$header.info$incrementX <- x3p$header.info$incrementX * scale_by
  x3p
}

#' Check resolution of a scan
#'
#' Scans in x3p format capture 3d topogographic surfaces. According to ISO standard
#' ISO5436 – 2000 scans are supposed to be captured in meters. For microscopic images
#' capture in meters might be impractical.
#' @param x3p object
#' @return numeric value of resolution per pixel
#' @export
x3p_get_scale <- function(x3p) {
  assert_that("x3p" %in% class(x3p))

  x3p$header.info$incrementX
}