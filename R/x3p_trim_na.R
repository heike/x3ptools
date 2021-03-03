#' Trim rows and columns with missing values only from an x3p 
#'
#' Trims rows and columns from the edges of a surface matrix that contain missing values only.
#' @param x3p x3p object
#' @return x3p object of the same or smaller dimension where missing values are removed from the boundaries
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' logo$surface.matrix[logo$surface.matrix == median(logo$surface.matrix)] <- NA
#' x3p_trim_na(logo) # reduced to dimension: 668 by 268
x3p_trim_na <- function(x3p) {
 # browser()
  rows <- apply(x3p$surface.matrix, MARGIN=1, FUN=function(x) sum(is.na(x)))
  idx <- which(rows == dim(x3p$surface.matrix)[2])
  if (idx[1] != 1) {
    xmin <- 1
  } else xmin <- max(which(idx==1:length(idx))) +1
  if (idx[length(idx)] != dim(x3p$surface.matrix)[1]) {
    xmax <- dim(x3p$surface.matrix)[1]
  } else xmax <- idx[min(which(idx==(dim(x3p$surface.matrix)[1] - rev(seq_along(idx))+1)))]-1
  
  cols <- apply(x3p$surface.matrix, MARGIN=2, FUN=function(x) sum(is.na(x)))
  idx <- which(cols == dim(x3p$surface.matrix)[1])
  
  if (idx[1] != 1) {
    ymin <- 1
  } else ymin <- max(which(idx==1:length(idx))) +1
  if (idx[length(idx)] != dim(x3p$surface.matrix)[2]) {
    ymax <- dim(x3p$surface.matrix)[2]
  } else ymax <- idx[min(which(idx==(dim(x3p$surface.matrix)[2] - rev(seq_along(idx))+1)))]-1
  
  x3p %>% x3p_crop(x = xmin, y = dim(x3p$surface.matrix)[2]-ymax, width = xmax-xmin, height = ymax-ymin)
}


