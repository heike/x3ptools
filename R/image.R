#' Raster image of an x3p surface
#'
#' This image function creates a quick raster image to show the surface of an x3p file.
#' Due to some inconsistency in where the origin (0,0) is supposed to be (top left or bottom left)  image functions from different packages will result in different images.
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
