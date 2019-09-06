#' Add a layer to the mask
#'
#' Add a region  a mask.  The region is specfied as TRUE values in a matrix of the same dimensions as the existing mask. In case no mask exists, one is created.
#' @param x3p x3p object
#' @param mask logical matrix of the same dimension as the surface matrix. Values of TRUE are assumed to be added in the mask, values of FALSE are being ignored.
#' @param color name or hex value of color
#' @param annotation character value describing the region
#' @return x3p object with changed mask
#' @export
x3p_add_mask_layer <- function(x3p, mask, color = "red", annotation = "") {
  stopifnot("x3p" %in% class(x3p))
  if (!exists("mask", x3p)) x3p <- x3p_add_mask(x3p, mask)

  # need to check that mask has the correct dimensions
  stopifnot(dim(mask) == dim(x3p$mask))

  x3p$mask[mask] <- color
  # add color and annotation to the xml file
  x3p <- x3p_add_annotation(x3p, color, annotation)

  x3p
}
