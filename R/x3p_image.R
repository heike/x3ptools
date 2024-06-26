#' Plot x3p object as an image
#'
#' Plot an interactive surface plot of the x3p matrix. This implementation uses the `rgl` package.
#' In case `rgl.useNULL` is set to TRUE (i.e. no separate window will be opened), an rgl widget 
#' can be used to show the surface in the viewer window (see the example). 
#' @param x3p x3p object
#' @param file file name for saving, if file is NULL the opengl device stays open.
#' The file extension determines the type of output. Possible extensions are png, stl (suitable for 3d printing), or svg.
#' @param col color specification
#' @param size vector of width and height. If only one value is given, height or width will be adjusted proportionally to the dimensions of the surface matrix of the scan to reach an upper bound of size.
#' @param zoom numeric value indicating the amount of zoom
#' @param multiply exaggerate the relief by factor multiply
#' @param update Boolean value indicating whether a scene should be updated (defaults to FALSE). If FALSE, a new rgl device is opened.
#' @param ... not used
#' @export
#' @import rgl
#' @importFrom rgl snapshot3d r3dDefaults
#' @examples
#' save <- getOption("rgl.useNULL")
#' options(rgl.useNULL=TRUE)
#' 
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' x3p_image(logo, size = c(741, 419), zoom=0.5)
#' # add crosscut:
#' logoplus <- x3p_add_hline(logo, yintercept = 50*.645e-6, color = "#e6bf98", size = 5)
#' x3p_image(logoplus, size = c(741, 419), zoom=0.5)
#' widget <- rgl::rglwidget()
#' if (interactive())
#'   widget
#'
#' options(rgl.useNULL=save)   
x3p_image <- function(x3p, file = NULL, col = "#cd7f32",
#                      crosscut = NA,
#                      ccParam = list(
#                        color = "#e6bf98",
#                        radius = 5
#                      ),
                      size = 750, zoom = 0.35, multiply = 5, update = FALSE, ...) {
  stopifnot("x3p" %in% class(x3p), is.numeric(size))
  surface <- x3p$surface.matrix
  z <- multiply * surface # Exaggerate the relief
  yidx <- ncol(z):1
  y <- x3p$header.info$incrementY * yidx #
  x <- x3p$header.info$incrementX * (1:nrow(z)) #

  if (all(is.na(size)) | length(size) == 1) {
    # set size to be proportional to value given if size is only one value
    if (length(size) == 1 & all(is.na(size))) size <- 750
    dims <- dim(surface)
    ratio <- dims[2]/dims[1]
    if (ratio < 1)
      size <- c(size[1], round(size[1]*dims[2]/dims[1]))
    else
      size <- c(round(size[1]*dims[1]/dims[2]), size[1])
  }

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
    pop3d("lights")
    light3d(
      x = xyz, diffuse = "gray40",
      specular = "gray40", ambient = "grey10", viewpoint.rel = TRUE
    )
    light3d(diffuse = "gray20", specular = "gray20")
  } else {
#    pop3d()
  }
  

  # if (!is.na(crosscut)) {
  #   .Deprecated("x3p_add_hline", msg = "Use of crosscut is deprecated. Use x3p_add_hline instead.")
  #   crosscutidx <- which.min(abs(crosscut - y))
  # 
  #   colmat <- matrix(rep(col, length(z)), nrow = nrow(z), ncol = ncol(z))
  #   if (exists("mask", x3p)) colmat <- as.vector(x3p$mask)
  # 
  #   if (length(crosscutidx) > 0) {
  #     coloridx <- pmax(crosscutidx - ccParam$radius, 0):pmin(crosscutidx + ccParam$radius, ncol(z))
  #     colmat[coloridx] <- ccParam$color 
  #     # I changed this to be [coloridx] instead of [, coloridx] because of the 
  #     # as.vector() call in the block above this if statement. 
  #   } else {
  #     warning(sprintf("Crosscut does not map to x3p file correctly. Crosscut is at %f, scan has height of %f", crosscut, max(y)))
  #   }
  # 
  #   if (crosscut > max(y)) {
  #     warning(sprintf("Crosscut does not map to x3p file correctly. Crosscut is at %f, scan has height of %f", crosscut, max(y)))
  #   }
  # 
  # 
  #   p <- surface3d(x, y, z, color = colmat, back = "fill")
  # } else {
    if (exists("mask", x3p)) col <- as.vector(x3p$mask)
    p <- surface3d(x, y, z, color = col, back = "fill")
#  }
  p
  if (!is.null(file)) {
    x3p_snapshot(file)
    close3d()
  }
  invisible(p) 
}

#' @export
#' @rdname x3p_image
image_x3p <- function(x3p, file = NULL, col = "#cd7f32",
#                      crosscut = NA,
#                      ccParam = list(
#                        color = "#e6bf98",
#                        radius = 5
#                      ),
                      size = 750, zoom = 0.35, multiply = 5, ...) {
  x3p_image(
    x3p = x3p, file = file, col = col, #crosscut = crosscut, ccParam = ccParam,
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

