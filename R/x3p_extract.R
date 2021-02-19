#' Extract values from a surface matrix based on a mask
#'
#' If a mask is present, a subset of the surface matrix is extracted based on specified value(s).
#' @param x3p x3p object
#' @param mask_vals vector of mask value(s) 
#' @return x3p object  
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' # add a mask
#' logo <- x3p_add_mask(logo)
#' mask <- t(logo$surface.matrix==median(logo$surface.matrix))
#' logo <- x3p_add_mask_layer(logo, mask, color = "red", annotation = "median") 
#' x3p_extract(logo, "#cd7f32") 
#' # x3p_image(logo, size=c(500,500), zoom = 1)
x3p_extract <- function(x3p, mask_vals) {
  if (is.null(x3p$mask)) return(x3p)
  
  idx <- which(!(t(as.matrix(x3p$mask)) %in% mask_vals))
  x3p$surface.matrix[idx] <- NA
  x3p
}



