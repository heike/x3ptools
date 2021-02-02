#' Flip the x coordinate of an x3p file
#'
#' Flip the surface matrix of an x3p file along the x axis.
#' @param x3p x3p object
#' @return x3p object in which the x coordinate is reversed.
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' dim(logo$surface.matrix)
#' \dontrun{
#' x3p_image(logo)
#' }
#' # flip the y-axis for the old ISO standard:
#' logoflip <- x3p_flip_x(logo)
#' dim(logoflip$surface.matrix)
#' \dontrun{
#' x3p_image(logoflip)
#' }
x3p_flip_x <- function(x3p) {

  x3p_transpose(rotate_x3p(x3p, angle = 90))
}

#' @rdname x3p_flip_x
#' @export
x_flip_x3p <- x3p_flip_x
