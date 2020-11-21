#' Transpose an x3p object
#'
#' Transpose the surface matrix of an x3p object. Also adjust meta information.
#' @param x3p x3p object
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' dim(logo$surface.matrix)
#' \dontrun{
#' x3p_image(logo)
#' }
#' #  transpose the image
#' logotp <- x3p_transpose(logo)
#' dim(logotp$surface.matrix)
#' \dontrun{
#' x3p_image(logotp)
#' }
x3p_transpose <- function(x3p) {
  stopifnot("x3p" %in% class(x3p))

  x3p$surface.matrix <- t(x3p$surface.matrix)

  size <- x3p$header.info$sizeX
  x3p$header.info$sizeX <- x3p$header.info$sizeY
  x3p$header.info$sizeY <- size

  inc <- x3p$header.info$incrementX
  x3p$header.info$incrementX <- x3p$header.info$incrementY
  x3p$header.info$incrementY <- inc

  x3p$matrix.info$MatrixDimension$SizeX[[1]] <- x3p$header.info$sizeX
  x3p$matrix.info$MatrixDimension$SizeY[[1]] <- x3p$header.info$sizeY

  if (!is.null(x3p$mask)) x3p$mask <- as.raster(t(as.matrix(x3p$mask)))

  x3p
}


#' @rdname x3p_transpose
#' @export
transpose_x3p <- function(x3p) {
  x3p_transpose(x3p)
}
