#' Flip the y coordinate of an x3p image
#'
#' One of the major changes between the previous two ISO standards is the
#' way the y axis is defined in a scan. The entry (0,0) used to refer to the top left corner of a scan, now it refers to the
#' bottom right corner, which means that all legacy x3p files have to flip their y axis in order to
#' conform to the newest ISO norm.
#' @param x3p x3p object
#' @return x3p object in which the y coordinate is reversed.
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' dim(logo$surface.matrix)
#' \dontrun{
#' x3p_image(logo)
#' }
#' # flip the y-axis for the old ISO standard:
#' logoflip <- x3p_flip_y(logo)
#' dim(logoflip$surface.matrix)
#' \dontrun{
#' x3p_image(logoflip)
#' }
x3p_flip_y <- function(x3p) {
  rotate_x3p(x3p_transpose(x3p), angle = 90)
}

#' @rdname x3p_flip_y
#' @export
y_flip_x3p <- x3p_flip_y
