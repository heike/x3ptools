#' Plot x3p object as an image
#' 
#' @param x3p x3p object
#' @param file file name for saving, if file is NULL the opengl device stays open. 
#' The file extension determines the type of output. Possible extensions are png, stl (suitable for 3d printing).
#' @param col color specification
#' @param size vector of width and height
#' @param zoom numeric value indicating the amount of zoom
#' @param multiply exaggerate the relief by factor multiply
#' @param ... not used
#' @export
#' @import rgl
#' @importFrom rgl snapshot3d r3dDefaults
#' @examples 
#' \dontrun{
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' image_x3p(logo, file = "logo.png")
#' }
image_x3p <- function(x3p, file = NULL, col = "#cd7f32", size = c(750, 250), zoom= 0.35, multiply=5, ...) {
  stopifnot("x3p" %in% class(x3p))
  surface <- x3p$surface.matrix
  z <- multiply*surface # Exaggerate the relief
  y <- x3p$header.info$incrementY * (ncol(z):1) # 
  x <- x3p$header.info$incrementX * (1:nrow(z)) # 
  
  params <- rgl::r3dDefaults
  #  params$viewport <- c(0,0, 750, 250)
  #  
  params$windowRect <- c(40, 125, 40+size[1], 125+size[2])
  params$userMatrix <- diag(c(1,1,1,1))
  params$zoom <- zoom
  
  open3d(params=params)
  rgl.pop("lights")
#  xyz <- matrix(c(-2000, mean(y), max(z, na.rm=TRUE)), ncol = 3)
  xyz <- matrix(c(min(y) - diff(range(y)), 
                  mean(y), max(z, na.rm=TRUE)), ncol = 3)
  
  light3d(x = xyz, diffuse = "gray40", 
          specular = "gray40", ambient="grey10", viewpoint.rel = TRUE) 
  light3d(diffuse = "gray20", specular = "gray20")
  
  surface3d(x, y, z, color = col, back = "lines")
  
  if (!is.null(file)) {
    splits <- strsplit(file, split ="\\.")
    extension <- splits[[1]][length(splits[[1]])]
    if (extension=="png") {
      rgl.snapshot(filename=file)
    }
    if (extension=="stl") {
      writeSTL(con=file)
    }
    rgl.close()
  }
}

