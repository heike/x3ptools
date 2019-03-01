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
#' image_x3p(logo, file = "logo.png", crosscut = 50)
#' }
image_x3p <- function(x3p, file = NULL, col = "#cd7f32",
                      crosscut = NA, 
                      ccParam = list(color = "#e6bf98",
                                     radius = 5),
                      size = c(750, 250), zoom = 0.35, multiply = 5, 
                      useNULL = FALSE, closeRGL = !is.null(file), ...) {
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
    crosscutidx <- which(yidx == crosscut)
    
    colmat <- matrix(rep(col, length(z)), nrow = nrow(z), ncol = ncol(z))
    
    if (length(crosscutidx) > 0) {
      coloridx <- pmax(crosscutidx - ccParam$radius, 0):pmin(crosscutidx + ccParam$radius, ncol(z))
      colmat[, coloridx] <- ccParam$color
    } else {
      warning("Crosscut does not map to x3p file correctly.")
    }
    
    rgl.open(useNULL = useNULL)
    surface3d(x, y, z, color = colmat, back = "fill")
  } else {
    rgl.open(useNULL = useNULL)
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
  }
  if (closeRGL) rgl.close()

}


#' Plot x3p object as an image
#'
#' @param x3p x3p object
#' @param file file name for saving, if file is NULL the opengl device stays open.
#' The file extension determines the type of output. Possible extensions are png, stl (suitable for 3d printing), or svg.
#' @param col color specification
#' @param spaces space between grid lines, doubled for x
#' @param gridParam list with named components, consisting of parameters for showing crosscuts:  color and radius for crosscut region
#' @param size vector of width and height
#' @param zoom numeric value indicating the amount of zoom
#' @param multiply exaggerate the relief by factor multiply
#' @param useNULL logical value. Do you want a new rgl window to be opened? Use useNull = FALSE (default) in interactive mode and useNULL = TRUE if the image is opened as part of a predefined space such as an image container in an html site.
#' @param closeRGL logical value. Should the RGL window be closed by the routine?
#' @param ... not used
#' @export
#' @import rgl
#' @importFrom rgl snapshot3d r3dDefaults
#' @examples
#' \dontrun{
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' image_x3p(logo, file = "logo.png", crosscut = 50)
#' }
image_x3p_grid <- function(x3p, file = NULL, col = "#cd7f32",
                      spaces = 50, 
                      gridParam = list(color = "#e6bf98",
                                      radius = 5),
                      size = c(750, 250), zoom = 0.35, multiply = 5, 
                      useNULL = FALSE, closeRGL = !is.null(file), ...) {
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
  
  if (!is.na(spaces)) {
 #   browser()
    yspaces = seq(1, max(yidx), by =spaces)
    crosscutidx <- which(yidx %in% yspaces)

    xidx <- 1:nrow(z)
    xspaces = seq(1, max(xidx), by =2*spaces)
    crosscutidy <- which(xidx %in% xspaces)
    
    colmat <- matrix(rep(col, length(z)), nrow = nrow(z), ncol = ncol(z))
    
    if (length(crosscutidx) > 0) {
      coloridx <- rep(crosscutidx, each = 2*gridParam$radius+1) + (-gridParam$radius):(gridParam$radius)
      coloridx <- unique(pmin(pmax(coloridx, 1), max(yidx)))

      coloridy <- rep(crosscutidy,  each = 2*gridParam$radius+1) + (-gridParam$radius):(gridParam$radius)
      coloridy <- unique(pmin(pmax(coloridy, 1), max(xidx)))

      colmat[, coloridx] <- gridParam$color
      colmat[coloridy, ] <- gridParam$color

   #   every five spaces:      
      yspaces = seq(1, max(yidx), by =5*spaces)
      crosscutidx <- which(yidx %in% yspaces)

      coloridx <- rep(crosscutidx, each = 2*gridParam$radius+1) + (-gridParam$radius):(gridParam$radius)
      coloridx <- unique(pmin(pmax(coloridx, 1), max(yidx)))
      
      xspaces = seq(1, max(xidx), by =5*2*spaces)
      crosscutidy <- which(xidx %in% xspaces)
      coloridy <- rep(crosscutidy,  each = 2*gridParam$radius+1) + (-gridParam$radius):(gridParam$radius)
      coloridy <- unique(pmin(pmax(coloridy, 1), max(xidx)))
     
      colmat[, coloridx] <- "darkred"
      colmat[coloridy, ] <- "darkred"
    
     #   every ten spaces:      
      yspaces = seq(1, max(yidx), by =10*spaces)
      crosscutidx <- which(yidx %in% yspaces)
      
      coloridx <- rep(crosscutidx, each = 2*gridParam$radius+1) + (-gridParam$radius):(gridParam$radius)
      coloridx <- unique(pmin(pmax(coloridx, 1), max(yidx)))
      
      xspaces = seq(1, max(xidx), by =10*2*spaces)
      crosscutidy <- which(xidx %in% xspaces)
      coloridy <- rep(crosscutidy,  each = 2*gridParam$radius+1) + (-gridParam$radius):(gridParam$radius)
      coloridy <- unique(pmin(pmax(coloridy, 1), max(xidx)))
      
      colmat[, coloridx] <- "black"
      colmat[coloridy, ] <- "black"  
    } else {
      warning("Crosscut does not map to x3p file correctly.")
    }
    
    rgl.open(useNULL = useNULL)
    surface3d(x, y, z, color = colmat, back = "fill")
  } else {
    rgl.open(useNULL = useNULL)
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
  }
  if (closeRGL) rgl.close()
}
