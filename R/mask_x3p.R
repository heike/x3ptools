#' Add/Exchange a mask for an x3p object
#'
#' Create a mask for an x3p object in case it does not have a mask yet.
#' Masks are used for overlaying colors on the bullets surface.
#' @param x3p x3p object
#' @param mask raster matrix of colors with the same dimensions as the x3p surface. If NULL, an object of the right size will be created.
#' @return x3p object with added/changed mask
#' @export
#' @importFrom grDevices as.raster
#' @examples
#' \dontrun{
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' color_logo <- magick::image_read(system.file("csafe-color.png", package="x3ptools"))
#' logoplus <- x3p_add_mask(logo, as.raster(color_logo))
#' image_x3p(logoplus, multiply=50, size = c(741, 419),zoom = 0.5)
#' }
x3p_add_mask <- function(x3p, mask = NULL) {
  stopifnot("x3p" %in% class(x3p))
  dims <- rev(dim(x3p$surface.matrix))
  if (is.null(mask)) mask <- as.raster(matrix("#cd7f32", dims[1], dims[2]))
  else {
    # check that the mask has the right dimensions
    if (!all(dim(mask) == dims)) {
      dm <- dim(mask)
      warning(sprintf("Mask does not have the right dimensions. Mask has dimensions %d x %d should be %d x %d.", dm[1], dm[2], dims[1], dims[2]))
    }
  }
  x3p$mask <- mask
  
  x3p
}

