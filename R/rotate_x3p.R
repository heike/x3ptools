#' Rotate an x3p object
#' 
#' Rotate the surface matrix of an x3p object. Also adjust meta information.
#' @param x3p x3p object
#' @param angle rotate counter-clockwise by angle degrees given as 90, 180, 270 degree (or -90, -180, -270). 
#' @export
#' @examples
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' dim(logo$surface.matrix)
#' \dontrun{
#' image_x3p(logo)
#' }
#' # rotate the image by 90 degrees clock-wise:
#' logo90 <- rotate_x3p(logo, 90)
#' dim(logo90$surface.matrix)
#' \dontrun{
#' image_x3p(logo90)
#' }

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
#' @examples
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' dim(logo$surface.matrix)
#' \dontrun{
#' image_x3p(logo)
#' }
#' #  transpose the image
#' logotp <- transpose_x3p(logo)
#' dim(logotp$surface.matrix)
#' \dontrun{
#' image_x3p(logotp)
#' }
transpose_x3p <- function(x3p) {
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
    
  
  x3p
}


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
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' dim(logo$surface.matrix)
#' \dontrun{
#' image_x3p(logo)
#' }
#' # flip the y-axis for the old ISO standard:
#' logoflip <- y_flip_x3p(logo)
#' dim(logoflip$surface.matrix)
#' \dontrun{
#' image_x3p(logoflip)
#' }
y_flip_x3p <- function(x3p) {
  rotate_x3p(transpose_x3p(x3p), angle=90)
}