#' Draw a quantile region on the mask
#'
#' For each x value of the surface matrix add a region to the mask of an x3p object corresponding to the area between two specified
#' quantiles.
#' @param x3p x3p object
#' @param quantiles vector of quantiles between which surface matrix values are included in the mask
#' @param color name or hex value of color
#' @param annotation character value describing the region
#' @return x3p object with changed mask
#' @importFrom stats quantile
#' @export
x3p_mask_quantile <- function(x3p, quantiles = c(0.25, 0.75), color = "red", annotation = "quantile-region") {
  stopifnot("x3p" %in% class(x3p))
  if (!exists("mask", x3p)) x3p <- x3p_add_mask(x3p)

  if (length(quantiles) == 1) quantiles <- c(quantiles, 1 - quantiles)

  tA <- apply(x3p$surface.matrix, 1, FUN = function(x) {
    qu <- quantile(x, na.rm = TRUE, probs = quantiles)
    (x >= qu[1]) & (x <= qu[2])
  })

  x3p$mask[tA] <- color
  # add color and annotation to the xml file
  x3p <- x3p_add_annotation(x3p, color, annotation)

  x3p
}
