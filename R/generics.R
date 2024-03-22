#' Raster image of an x3p surface
#'
#' `image.x3p` expands the generic image method for x3p objects.
#' This image function creates a raster image to show the surface of an x3p file.
#' Due to some inconsistency in the mapping of the origin (0,0), (choice between top left or bottom left)  image functions from different packages will result in different images.
#' @param x an x3p object
#' @param ... parameters passed into image
#' @importFrom graphics image
#' @method image x3p
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' image(logo)
image.x3p <- function(x, ...) {
  graphics::image(x$surface.matrix, ylim = c(1, 0), ...)
}


#' Dimension of an x3p object
#'
#' `dim.x3p` expands the generic dim method for x3p objects and returns (mainly) the dimensions of the surface matrix.  
#' @param x an x3p object
#' @method dim x3p
#' @returns a vector of length three. The first two integers contain width and height of the surface scan, the third dimension is either 1 or 2, depending on whether there is a mask (2) or not (1). 
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' dim(logo)
dim.x3p <- function(x) {
  matrix_dims <- dim(x$surface.matrix)
  z <- 1
  if (!is.null(x$mask)) z <- 2
  c(matrix_dims, z)
}
