#' Show meta information of an x3p file
#'
#' `head.x3p` expands the generic head method for x3p objects. It gives a summary of the most relevant 3p meta information and returns the object invisibly.
#' @param x x3p object
#' @param n number of rows/columns of the matrix
#' @param ... extra parameters passed to head.matrix()
#' @importFrom utils head
#' @importFrom utils head.matrix
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' head(logo)
#' @method head x3p
head.x3p <- function(x, n = 6L, ...) {
  cat(sprintf(
    "size (width x height): %d x %d in pixel \n",
    x$header.info$sizeX, x$header.info$sizeY
  ))
  cat(sprintf(
    "resolution: %.4e x %.4e \n", x$header.info$incrementX,
    x$header.info$incrementY
  ))
  head.matrix(x$surface.matrix[, 1:n], n = n, ...)
}
