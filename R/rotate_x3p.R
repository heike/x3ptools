#' Rotate an x3p object
#' 
#' Rotate the surface matrix of an x3p object. Also adjust meta information.
#' @param x3p x3p object
#' @param angle rotate counter-clockwise by angle degrees given as 90, 180, 270 degree (or -90, -180, -270). 
#' @importFrom raster as.raster
#' @export
#' @examples
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' dim(logo$surface.matrix)
#' \dontrun{
#' image_x3p(logo)
#' }
#' # rotate the image by 90 degrees counter-clockwise:
#' logo90 <- rotate_x3p(logo, 90)
#' dim(logo90$surface.matrix)
#' \dontrun{
#' image_x3p(logo90)
#' }
x3p_rotate <- function(x3p, angle=90) {
  times <- (angle %/% 90) %% 4
  if (times == 0) return (x3p)
  
  # counterr clockwise rotation by 90 degrees
  for (i in 1:times) {
    x3p$surface.matrix <- t(apply(x3p$surface.matrix, 2, rev))
    if (!is.null(x3p$mask)) x3p$mask <- as.raster(apply(t(as.matrix(x3p$mask)), 2, rev))

    size <- x3p$header.info$sizeX
    x3p$header.info$sizeX <- x3p$header.info$sizeY
    x3p$header.info$sizeY <- size
    
    inc <- x3p$header.info$incrementX
    x3p$header.info$incrementX <- x3p$header.info$incrementY
    x3p$header.info$incrementY <- inc
    
    x3p$matrix.info$MatrixDimension$SizeX[[1]] <- x3p$header.info$sizeX
    x3p$matrix.info$MatrixDimension$SizeY[[1]] <- x3p$header.info$sizeY
    
  }
  
 
  x3p
}


#' @rdname x3p_rotate
#' @export
rotate_x3p <- function(x3p, angle=90) {
  x3p_rotate(x3p=x3p, angle=angle)
}