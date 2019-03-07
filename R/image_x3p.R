#' Plot x3p object as an image
#'
#' @param x3p x3p object
#' @param file file name for saving, if file is NULL the opengl device stays open.
#' The file extension determines the type of output. Possible extensions are png, stl (suitable for 3d printing), or svg.
#' @param col color specification
#' @param crosscut crosscut index
#' @param ccParam list with named components, consisting of parameters for showing crosscuts:  color and radius for crosscut region
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
#' image_x3p(logo, file = "logo.png", crosscut = 50*.645e-6)
#' # alternative to crosscut
#' logoplus <- x3p_add_hline(logo, yintercept = 50*.645e-6, color = "#e6bf98", size = 5)
#' image_x3p(logoplus, size = c(741, 419), zoom=0.5)
#' }
image_x3p <- function(x3p, file = NULL, col = "#cd7f32",
                      crosscut = NA, 
                      ccParam = list(color = "#e6bf98",
                                     radius = 5),
                      size = c(750, 250), zoom = 0.35, multiply = 5, ...) {
  stopifnot("x3p" %in% class(x3p))
  surface <- x3p$surface.matrix
  z <- multiply * surface # Exaggerate the relief
  yidx <- ncol(z):1
  y <- x3p$header.info$incrementY * yidx #
  x <- x3p$header.info$incrementX * (1:nrow(z)) #


  params <- rgl::r3dDefaults
  #  params$viewport <- c(0,0, 750, 250)
  #
  params$windowRect <- c(40, 125, 40 + size[1], 125 + size[2])
  params$userMatrix <- diag(c(1, 1, 1, 1))
  params$zoom <- zoom

  open3d(params = params)
  rgl.pop("lights")
  #  xyz <- matrix(c(-2000, mean(y), max(z, na.rm=TRUE)), ncol = 3)
  xyz <- matrix(c(
    min(y) - diff(range(y)),
    mean(y), max(z, na.rm = TRUE)
  ), ncol = 3)

  light3d(
    x = xyz, diffuse = "gray40",
    specular = "gray40", ambient = "grey10", viewpoint.rel = TRUE
  )
  light3d(diffuse = "gray20", specular = "gray20")

  if (!is.na(crosscut)) {
    .Deprecated("x3p_add_hline", msg = "Use of crosscut is deprecated. Use x3p_add_hline instead.")
    crosscutidx <- which.min(abs(crosscut - y))
    
    colmat <- matrix(rep(col, length(z)), nrow = nrow(z), ncol = ncol(z))
    if (exists("mask", x3p)) colmat <- as.vector(x3p$mask)
    
    if (length(crosscutidx) > 0) {
      coloridx <- pmax(crosscutidx - ccParam$radius, 0):pmin(crosscutidx + ccParam$radius, ncol(z))
      colmat[, coloridx] <- ccParam$color
    } else {
      warning(sprintf("Crosscut does not map to x3p file correctly. Crosscut is at %f, scan has height of %f", crosscut, max(y)))
    }
    
    if (crosscut > max(y))
      warning(sprintf("Crosscut does not map to x3p file correctly. Crosscut is at %f, scan has height of %f", crosscut, max(y)))

    
    surface3d(x, y, z, color = colmat, back = "fill")
  } else {
    if (exists("mask", x3p)) col <- as.vector(x3p$mask)
    surface3d(x, y, z, color = col, back = "fill")
  }
  
  if (!is.null(file)) {
    splits <- strsplit(file, split = "\\.")
    extension <- splits[[1]][length(splits[[1]])]
    if (extension == "png") {
      rgl.snapshot(filename = file)
    }
    if (extension == "svg") {
      rgl.postscript(filename = file, fmt = "svg")
    }
    if (extension == "stl") {
      writeSTL(con = file)
    }
    rgl.close()
  }
}


