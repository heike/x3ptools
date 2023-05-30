#' Shade the mask of an x3p object to reflect its surface profile 
#' 
#' Apply color shading to the mask of a 3d topographic surface. 
#' @param x3p object containing a 3d topographic surface 
#' @param colors vector of colors
#' @param freqs vector of values corresponding to color frequency (turned into quantiles of the differenced values)
#' @return x3p object with color-shaded mask
#' @export
#' @examples 
#' \dontrun{
#' x3p <- x3p_read("~/Documents/CSAFE/Wirecutter/data/Aluminum Wires renamed/T1AW-LI-R1-B01.x3p")
#' x3p_image(x3p, size = dim(x3p$surface.matrix)/5, zoom=0.8)
#' x3p_with <- x3p %>% x3p_shade_mask()
#' x3p_image(x3p_with %>% x3p_sample(5), size = dim(x3p$surface.matrix)/5, zoom=0.8)
#' # sampling first is better at preserving the intended color frequency
#' x3p_with2 <- x3p %>% x3p_sample(5) %>% x3p_shade_mask()
#' x3p_image(x3p_with2, size = dim(x3p$surface.matrix)/5, zoom=0.8)
#' x3p_with3 <- x3p %>% x3p_average(5) %>% x3p_shade_mask()
#' x3p_image(x3p_with3, size = dim(x3p$surface.matrix)/5, zoom=0.8)
#' 
#' lea <- x3p_read("~/Documents/Data/Houston Persistence/Barrel A/Bullet 11/Houston 1 - Barrel A - Bullet 11 - Land 1 - Sneox1 - 20x - auto light left image + 20% x10 - threshold 2 - resolution 4 - Mark Mosher.x3p")
#' lea %>% x3p_sample(m=5) %>% x3p_shade_mask() %>% x3p_image()
#' lea %>% x3p_sample(m=5) %>% x3p_shade_mask(freqs = c(0, 0.05, 0.1, 0.2,0.8, 0.9, 0.95, 1)) %>% x3p_image()
#' }
x3p_shade_mask <- function(x3p, 
  colors = rev(c("#b12819", "#d7301f","#e16457","#ffffff","#5186a2","#175d82", "#134D6B")), 
  freqs = c(0, 0.05, 0.25, 0.45, 0.55, 0.75, 0.95, 1)) {
  stopifnot("x3p" %in% class(x3p))
  stopifnot(length(freqs) >= 3) # we need at least min, max and one other value. 
  stopifnot(length(colors) == length(freqs) -1)
  freqs <- sort(freqs) # just to prevent any strange things
    

  rescaled <- scales::rescale(x3p$surface.matrix)
  qus = quantile(rescaled, freqs, na.rm=TRUE)
#  hexes <- scales::gradient_n_pal(colors, qus)(rescaled)
  hexes <- cut(rescaled, breaks=qus, labels=colors)
  
  dims <- dim(x3p$surface.matrix)
  stripes <- matrix(hexes, byrow = FALSE, nrow = dims[1])
  
  x3p <- x3p %>% x3p_add_mask(mask = as.raster(t(stripes)))
  
  #x3p_image(x3p_diff, size = dim(x3p_diff$surface.matrix)/5, zoom=0.8)
  
  
  x3p
}