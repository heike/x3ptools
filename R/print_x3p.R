#' Show meta information of an x3p file
#'
#' `print.x3p` expands the generic print method for x3p objects. It gives a summary of the most relevant x3p meta information and returns the object invisibly.
#' @param x x3p object
#' @param ... ignored
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' print(logo)
#' @method print x3p
print.x3p <- function(x, ...) {
  cat("x3p object\n")
  cat(sprintf("Instrument: %s \n", x$general.info$Instrument$Manufacturer[[1]]))
  cat(sprintf(
    "size (width x height): %d x %d in pixel \n",
    x$header.info$sizeX, x$header.info$sizeY
  ))
  cat(sprintf(
    "resolution: %.4e x %.4e \n", x$header.info$incrementX,
    x$header.info$incrementY
  ))
  cat(sprintf("Creator: %s \n", x$general.info$Creator[[1]]))
  cat(sprintf("Comment: %s \n", x$general.info$Comment[[1]]))
  invisible(x)
}
