#' Get legend for mask colors
#' 
#' Retrieve  color definitions and annotations from the mask. If available, results in a named vector of colors. 
#' @param x3p x3p object with a mask
#' @return named vector of colors, names show annotations. In case no annotations exist NULL is returned.
#' @export
#' @examples
#' x3p <- read_x3p(system.file("sample-land.x3p", package="x3ptools"))
#' x3p_mask_legend(x3p) # annotations and color hex definitions
x3p_mask_legend <- function(x3p) {
  stopifnot("x3p" %in% class(x3p)) # no x3p object
  if (is.null(x3p$mask)) return(NULL)
  if (length(suppressWarnings(x3p_show_xml(x3p, "Annotations"))) == 0) return(NULL)
  
  annotations <- unlist(x3p$matrix.info$Mask$Annotations)
  colors <- unlist(lapply(x3p$matrix.info$Mask$Annotations, attributes))
  names(colors) <- annotations
  colors
}

#' Add legend to active rgl object
#' 
#' Add the legend for colors and annotations to the actuve rgl window.
#' @param x3p x3p object with a mask
#' @param colors named character vector of colors (in hex format by default), names contain annotations
#' @export
#' @examples 
#' x3p <- read_x3p(system.file("sample-land.x3p", package="x3ptools"))
#' \dontrun{
#' image_x3p(x3p) # run when rgl can open window on the device
#' x3p_add_legend(x3p) # add legend
#' }
x3p_add_legend <- function(x3p, colors = NULL) {
  stopifnot("x3p" %in% class(x3p)) # no x3p object
  stopifnot(length(rgl.dev.list()) > 0)
  
  if (is.null(colors)) colors <- x3p_mask_legend(x3p) 
  if (!is.null(colors))
    legend3d("bottomright", legend = names(colors), pch = 15, 
             col =colors,
             cex=1, pt.cex = 2, inset=c(0.02))
}



#' Lighten active rgl object
#' 
#' Make the currently active rgl object lighter. Adds a light source. Up to eight light sources can be added. Alternatively, any rgl light source can be added (see `light3d`).
#' @export
#' @examples 
#' x3p <- read_x3p(system.file("sample-land.x3p", package="x3ptools"))
#' \dontrun{
#' image_x3p(x3p) # run when rgl can open window on the device
#' x3p_lighter() # add a light source
#' }
x3p_lighter <- function() {
  stopifnot(length(rgl.dev.list()) > 0)
  # stopifnot("x3p" %in% class(x3p)) # no x3p object
  
  light3d(diffuse = "gray20", specular = "gray20")
}

#' Darken active rgl object
#' 
#' Makes the currently active rgl object darker by removing a light source. Once all light sources are removed the object can not be any darker.
#' @export
#' @examples 
#' x3p <- read_x3p(system.file("sample-land.x3p", package="x3ptools"))
#' \dontrun{
#' image_x3p(x3p) # run when rgl can open window on the device
#' x3p_darker() # remove a light source
#' }
x3p_darker <- function() {
  stopifnot(length(rgl.dev.list()) > 0)
  # stopifnot("x3p" %in% class(x3p)) # no x3p object
  
  rgl.pop("lights")
}