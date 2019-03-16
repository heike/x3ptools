#' Get legend for mask colors
#' 
#' @param x3p x3p object (with a mask)
#' @return named vector of colors, names show annotations
#' @export
x3p_mask_legend <- function(x3p) {
  stopifnot("x3p" %in% class(x3p)) # no x3p object
  stopifnot(!is.null(x3p$mask)) # no mask
  stopifnot(length(x3p_show_xml(x3p, "Annotations")) > 0) # no annotations
  
  annotations <- unlist(x3p$matrix.info$Mask$Annotations)
  colors <- unlist(lapply(x3p$matrix.info$Mask$Annotations, attributes))
  names(colors) <- annotations
  colors
}