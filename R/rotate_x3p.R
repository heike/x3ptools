#' Rotate an x3p object
#' 
#' Rotate the surface matrix of an x3p object. Also adjust meta information.
#' @param x3p x3p object
#' @param angle rotate counter-clockwise by angle degrees given as 90, 180, 270 degree (or -90, -180, -270). 
#' @export
rotate_x3p <- function(x3p, angle=90) {
  times <- (angle %/% 90) %% 4
  if (times == 0) return (x3p)
  
  # clockwise rotation by 90 degrees
  for (i in 1:times) {
    x3p$surface.matrix <- t(apply(x3p$surface.matrix, 2, rev))

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


#' Transpose an x3p object
#' 
#' Transpose the surface matrix of an x3p object. Also adjust meta information.
#' @param x3p x3p object
#' @export
transpose_x3p <- function(x3p) {

  x3p$surface.matrix <- t(x3p$surface.matrix)
    
    size <- x3p$header.info$sizeX
    x3p$header.info$sizeX <- x3p$header.info$sizeY
    x3p$header.info$sizeY <- size
    
    inc <- x3p$header.info$incrementX
    x3p$header.info$incrementX <- x3p$header.info$incrementY
    x3p$header.info$incrementY <- inc
    
    x3p$matrix.info$MatrixDimension$SizeX[[1]] <- x3p$header.info$sizeX
    x3p$matrix.info$MatrixDimension$SizeY[[1]] <- x3p$header.info$sizeY
    
  
  x3p
}