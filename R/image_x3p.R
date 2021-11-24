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
#' @param update Boolean value indicating whether a scene should be updated (defaults to FALSE). If FALSE, a new rgl device is opened.
#' @param ... not used
#' @export
#' @import rgl
#' @importFrom rgl snapshot3d r3dDefaults
#' @examples
#' \dontrun{
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' x3p_image(logoplus, size = c(741, 419), zoom=0.5)
#' # add crosscut:
#' logoplus <- x3p_add_hline(logo, yintercept = 50*.645e-6, color = "#e6bf98", size = 5)
#' x3p_image(logoplus, size = c(741, 419), zoom=0.5)
#' }
x3p_image <- function(x3p, file = NULL, col = "#cd7f32",
                      crosscut = NA,
                      ccParam = list(
                        color = "#e6bf98",
                        radius = 5
                      ),
                      size = c(750, 250), zoom = 0.35, multiply = 5, update = FALSE, ...) {
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

  
  #  xyz <- matrix(c(-2000, mean(y), max(z, na.rm=TRUE)), ncol = 3)
  xyz <- matrix(c(
    min(y) - diff(range(y)),
    mean(y), max(z, na.rm = TRUE)
  ), ncol = 3)
  
  if (!update) { 
    open3d(params = params)
    rgl.pop("lights")
    light3d(
      x = xyz, diffuse = "gray40",
      specular = "gray40", ambient = "grey10", viewpoint.rel = TRUE
    )
    light3d(diffuse = "gray20", specular = "gray20")
  } else {
#    pop3d()
  }
  

  if (!is.na(crosscut)) {
    .Deprecated("x3p_add_hline", msg = "Use of crosscut is deprecated. Use x3p_add_hline instead.")
    crosscutidx <- which.min(abs(crosscut - y))

    colmat <- matrix(rep(col, length(z)), nrow = nrow(z), ncol = ncol(z))
    if (exists("mask", x3p)) colmat <- as.vector(x3p$mask)

    if (length(crosscutidx) > 0) {
      coloridx <- pmax(crosscutidx - ccParam$radius, 0):pmin(crosscutidx + ccParam$radius, ncol(z))
      colmat[coloridx] <- ccParam$color 
      # I changed this to be [coloridx] instead of [, coloridx] because of the 
      # as.vector() call in the block above this if statement. 
    } else {
      warning(sprintf("Crosscut does not map to x3p file correctly. Crosscut is at %f, scan has height of %f", crosscut, max(y)))
    }

    if (crosscut > max(y)) {
      warning(sprintf("Crosscut does not map to x3p file correctly. Crosscut is at %f, scan has height of %f", crosscut, max(y)))
    }


    surface3d(x, y, z, color = colmat, back = "fill")
  } else {
    if (exists("mask", x3p)) col <- as.vector(x3p$mask)
    surface3d(x, y, z, color = col, back = "fill")
  }

  if (!is.null(file)) {
    x3p_snapshot(file)
    rgl.close()
  }
  
}

#' @export
#' @rdname x3p_image
image_x3p <- function(x3p, file = NULL, col = "#cd7f32",
                      crosscut = NA,
                      ccParam = list(
                        color = "#e6bf98",
                        radius = 5
                      ),
                      size = c(750, 250), zoom = 0.35, multiply = 5, ...) {
  x3p_image(
    x3p = x3p, file = file, col = col, crosscut = crosscut, ccParam = ccParam,
    size = size, zoom = zoom, multiply = multiply, ...
  )
}

#' Take a snapshot of the active rgl device and save in a file
#'
#' Make a snapshot of the current rgl device and save it to file. Options for file formats are png, svg, and stl (for 3d printing).
#' @param file file name for saving.
#' The file extension determines the type of output. Possible extensions are png, stl (suitable for 3d printing), or svg.
#' @export
x3p_snapshot <- function(file) {
  if (!is.null(file)) {
    splits <- strsplit(file, split = "\\.")
    extension <- splits[[1]][length(splits[[1]])]
    if (extension == "png") {
      rgl.snapshot(filename = file, top = TRUE)
    }
    if (extension == "svg") {
      rgl.postscript(filename = file, fmt = "svg")
    }
    if (extension == "stl") {
      writeSTL(con = file)
    }
  }
}

#' Raster image of an x3p surface
#'
#' `image.x3p` expands the generic image method for x3p objects.
#' This image function creates a raster image to show the surface of an x3p file.
#' Due to some inconsistency in the mapping of the origin (0,0), (choice between top left or bottom left)  image functions from different packages will result in different images.
#' @param x an x3p object
#' @param ... parameters passed into image
#' @importFrom graphics image
#' @method image x3p
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' image(logo)
image.x3p <- function(x, ...) {
  graphics::image(x$surface.matrix, ylim = c(1, 0), ...)
}
