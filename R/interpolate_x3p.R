#' Interpolate from an x3p object
#' 
#' An interpolated scan is created at specified resolutions `resx`, `resy` in x and y direction.
#' The interpolation is based on `na.approx` from the `zoo` package. It is possible to create interpolations at a higher resolution than the one specified in the data itself, but not recommended to do so.
#' `interpolate_x3p` can also be used as a way to linearly interpolate any missing values in an existing scan without changing the resolution.
#' @param x3p x3p object
#' @param resx numeric value specifying the new resolution for the x axis.
#' @param resy numeric value specifying the new resolution for the y axis.
#' @param maxgap integer variable used in `na.approx` to specify the maximum number of NAs to be interpolated, defaults to 1.
#' @return interpolated x3p object
#' @importFrom zoo na.approx 
#' @export
interpolate_x3p <- function(x3p, resx=1e-6, resy=resx, maxgap=1) {
  stopifnot("x3p" %in% class(x3p))
  if ((resx < x3p$header.info$incrementX) | (resy < x3p$header.info$incrementY)) 
    warning("New resolution is higher than the old. proceed with caution.\n")

  sizes <- dim(x3p$surface.matrix)
  newsize = round(sizes*c(x3p$header.info$incrementX/resx, x3p$header.info$incrementY/resy))
  seqx <- seq.int(from = 1, to = sizes[1]) 
  
  newseq <- seq.int(from = 1, to = newsize[1]) * resx/x3p$header.info$incrementX
  matrix1 <- apply(x3p$surface.matrix, 2, na.approx, 
        x = seqx, xout=newseq, na.rm = FALSE, maxgap=maxgap)

  seqy <- seq.int(from = 1, to = sizes[2]) 

  newseq <- seq.int(from = 1, to = newsize[2]) * resy/x3p$header.info$incrementY
  matrix2 <- t(apply(matrix1, 1, na.approx, 
                   x = seqy, xout=newseq, na.rm = FALSE, maxgap=maxgap))
  
  x3p$surface.matrix <- matrix2
  
  x3p$header.info$sizeX <- nrow(x3p$surface.matrix)
  x3p$header.info$sizeY <- ncol(x3p$surface.matrix)
  
  x3p$header.info$incrementX <- resx
  x3p$header.info$incrementY <- resy
  
  x3p$matrix.info$MatrixDimension$SizeX[[1]] <- x3p$header.info$sizeX
  x3p$matrix.info$MatrixDimension$SizeY[[1]] <- x3p$header.info$sizeY
  
  
  x3p
}