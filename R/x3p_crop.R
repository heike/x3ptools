#' Crop an x3p object to a specified width and height
#'
#' Cuts out a rectangle of size width x height from the location (x, y) of an x3p object. 
#' x and y specify the bottom right corner of the rectangle. 
#' In case the dimensions of the surface matrix do not allow for the full dimensions of the rectangle cutout the dimensions are adjusted accordingly.
#' @param x3p x3p object
#' @param x integer, location (in pixels) of the leftmost side of the rectangle,
#' @param y integer, location (in pixels) of the leftmost side of the rectangle,
#' @param width integer, width (in pixels) of the rectangle,
#' @param height integer, height (in pixels) of the rectangle,
#' @return x3p object of the cropped location expanded by list element `offset` that contains the coordinates of the cropped piece within the parent.
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' # crop the x3p file to just the CSAFE logo
#' logo_only <- x3p_crop(logo, x=20, y=50, width = 255 ,height =310)
#' logo_only <- x3p_crop(logo, x=20, y=50, width = 255 ,height =510)
#' # x3p_image(logo_only, size=c(500,500), zoom = 1)
x3p_crop <- function(x3p, x=1, y=1, width=128, height=128) {
  # check that x, x+width, y, y+height are inside the matrix
  y3p <- x3p
  dims <- dim(y3p$surface.matrix)
  xidx <- x+0:(width-1)
  yidx <- sort(dims[2] - (y+0:(height-1)))+1
  # dimension checks:
  xidx <- xidx[(xidx > 0) & (xidx <= dims[1])]
  yidx <- yidx[(yidx > 0) & (yidx <= dims[2])]
  # warn in case dimensions are not what was specified by user:
  if ((length(xidx) != width) | (length(yidx) != height)) {
    warning(sprintf("Specified dimension are adjusted to fit within range of the surface matrix: %d x %d", length(xidx), length(yidx)))
    height <- length(yidx)
    width <-  length(xidx)          
  }
  stopifnot(length(xidx) > 0, length(yidx) > 0) # there should be something left over after adjusting
  
  y3p$surface.matrix <- y3p$surface.matrix[xidx, yidx]  
  y3p$header.info$sizeY <- height
  y3p$header.info$sizeX <- width
  y3p$matrix.info$MatrixDimension$SizeX[[1]] <- y3p$header.info$sizeX
  y3p$matrix.info$MatrixDimension$SizeY[[1]] <- y3p$header.info$sizeY
  
  # identify location of the cropped piece within (original) parent
  xmin_prev <- 0
  ymin_prev <- 0
  if (!is.null(x3p$offset)) {
    prev_offset <- x3p$offset
    xmin_prev <- prev_offset[1,1]-1
    ymin_prev <- prev_offset[1,2]-1
  }
  dims <- c(xmin = xmin_prev + x, 
            ymin = ymin_prev + y,
            xmax = xmin_prev+x+width-1,
            ymax = ymin_prev+y+height-1)
  # scale indices to current scale. 
  # HH: I believe that we need to make sure to rescale or toss the offset info when we sample or average
  scale <- y3p %>% x3p_get_scale()
  scale_idx <- function(index) {scale*(index-1)}
  
  scaled_dims <- scale_idx(dims)
  offset <- rbind(dims, scaled_dims)
  offset <- data.frame(offset, units = c("index", "scaled"))
  y3p$offset <- offset
  
  y3p <- y3p %>% x3p_paste_comment(paste0("cropped from location (", x, ",", y,")"), sep="; ")
  
  if (!is.null(y3p$mask)) {
    # matrix with flipped x and y dimensions
    y3p$mask <- y3p$mask[yidx, xidx]
  }
  y3p
}



