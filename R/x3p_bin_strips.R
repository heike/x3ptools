#' Add colored stripes of the surface gradient to the mask of an x3p object
#' 
#' Apply gradient-based color shading to the mask of a 3d topographic surface. 
#' Gradients are determined empirically based on sequentical row- (or column-)wise 
#' differences of surface values. 
#' The `direction` parameter determines the direction of differencing. 
#' If direction is "vertical", columns in the surface matrix are 
#' differenced to identify whether 'vertical' stripes exist. 
#' @param x3p object containing a 3d topographic surface 
#' @param direction in which the stripes are created: `vertical` or `horizontal`.
#' @param colors vector of colors
#' @param freqs vector of values corresponding to color frequency (turned into quantiles of the differenced values)
#' @return x3p object with mask colored by discretized surface gradient
#' @export
#' @examples 
#' data(wire)
#' x3p <- wire
#' if (interactive()) x3p_image(x3p, size = c(400, 400), zoom=0.8)
#' x3p_with <- x3p_bin_stripes(x3p, direction="vertical")
#' x3p_with <- x3p_bin_stripes(x3p, direction="vertical", 
#' colors=c("#b12819","#ffffff","#134D6B"), freqs=c(0, 0.3, 0.7, 1))
#' if (interactive()) x3p_image(x3p_with, size = c(400, 400), zoom=0.8)
#' 
#' data(lea)
#' if (interactive()) {
#'   lea %>% x3p_bin_stripes() %>% x3p_image() # default stripes
#'  
#' # three colors only   
#'   lea %>% x3p_bin_stripes(
#'      colors=c("#b12819","#ffffff","#134D6B"), 
#'      freqs=c(0, 0.3, 0.7, 1)) %>%  x3p_image()
#' }
x3p_bin_stripes <- function(x3p, direction = "vertical", 
  colors = rev(c("#b12819", "#d7301f","#e16457","#ffffff","#5186a2","#175d82", "#134D6B")), 
  freqs = c(0, 0.05, 0.1, 0.3,0.7, 0.9, 0.95, 1)) {
  stopifnot("x3p" %in% class(x3p))
  stopifnot(length(freqs) >= 3) # we need at least min, max and one other value. 
  stopifnot(length(colors) == length(freqs) -1)
  stopifnot(direction %in% c("vertical", "horizontal"))
  freqs <- sort(freqs) # just to prevent any strange things
    
  x3p_diff <- x3p
  dims <- dim(x3p_diff$surface.matrix)
  
  if (direction=="horizontal") {
    x3p_diff$surface.matrix <- x3p_diff$surface.matrix[,-1]-x3p_diff$surface.matrix[,-dims[2]]
    x3p_diff$header.info$sizeY <-  x3p_diff$header.info$sizeY-1
  }
  if (direction=="vertical") {
    x3p_diff$surface.matrix <- x3p_diff$surface.matrix[-1,]-x3p_diff$surface.matrix[-dims[1],]
    x3p_diff$header.info$sizeX <-  x3p_diff$header.info$sizeX-1
  }

  rescaled <- scales::rescale(x3p_diff$surface.matrix)
  qus = quantile(rescaled, freqs, na.rm=TRUE)
#  hexes <- scales::gradient_n_pal(colors, qus)(rescaled)
  hexes <- cut(rescaled, breaks=qus, labels=colors)

  if (direction=="horizontal") {
    stripes <- matrix(hexes, 
                      byrow = FALSE, nrow = dims[1])
    stripes <- cbind(stripes, NA)
  }
  if (direction=="vertical") {
    stripes <- matrix(hexes, 
                      byrow = FALSE, ncol = dims[2])
    stripes <- rbind(stripes, NA)
  }
  
  x3p <- x3p %>% x3p_add_mask(mask = as.raster(t(stripes)))
  
  #x3p_image(x3p_diff, size = dim(x3p_diff$surface.matrix)/5, zoom=0.8)
  
  
  x3p
}
