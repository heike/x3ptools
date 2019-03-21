#' Add/Exchange a mask for an x3p object
#'
#' Create a mask for an x3p object in case it does not have a mask yet.
#' Masks are used for overlaying colors on the bullets surface.
#' @param x3p x3p object
#' @param mask raster matrix of colors with the same dimensions as the x3p surface. If NULL, an object of the right size will be created.
#' @return x3p object with added/changed mask
#' @export
#' @importFrom grDevices as.raster
#' @examples
#' x3p <- read_x3p(system.file("sample-land.x3p", package="x3ptools"))
#' # x3p file has mask consisting color raster image:
#' x3p$mask[1:5,1:5]
#' \dontrun{
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' color_logo <- png::readPNG(system.file("csafe-color.png", package="x3ptools"))
#' logoplus <- x3p_add_mask(logo, as.raster(color_logo))
#' image_x3p(logoplus, multiply=50, size = c(741, 419),zoom = 0.5)
#' }
x3p_add_mask <- function(x3p, mask = NULL) {
  stopifnot("x3p" %in% class(x3p))
  dims <- rev(dim(x3p$surface.matrix))
  if (is.null(mask)) {  
    if (!"Mask" %in% names(x3p$matrix.info)) {
      x3p$matrix.info$Mask <- list(
        Background = if (length(unique(mask)) == 1) {list(unique(mask))} else {list("")},
        Annotations = list()
      )
    }
    mask <- as.raster(matrix("#cd7f32", dims[1], dims[2]))
  } else {
    mask <- as.raster(mask) # Fix matrix/array rasters
    # check that the mask has the right dimensions
    if (!all(dim(mask) == dims)) {
      dm <- dim(mask)
      # SVP: This warning doesn't handle extra matrix dimensions :)
      warning(sprintf("Mask does not have the right dimensions. Mask has dimensions %d x %d should be %d x %d.", dm[1], dm[2], dims[1], dims[2]))
    }
  }
  x3p$mask <- mask
  
  # This is necessary so that mask information can be added
  if (!"matrix.info" %in% names(x3p)) {
    x3p$matrix.info <- list(MatrixDimension = list(SizeX = dim(x3p$surface.matrix)[1],
                                                   SizeY = dim(x3p$surface.matrix)[2],
                                                   SizeZ = 1))
  }
  
  x3p
}

#' Delete mask from an x3p object
#' 
#' Deletes mask and its annotations from an x3p file.
#' @param x3p x3p object
#' @return x3p object without the mask
#' @export
x3p_delete_mask <- function(x3p) {
  x3p$mask <- NULL # delete mask
  # also delete annotations from XML file

  if ("Mask" %in% names(x3p$matrix.info)) {
    x3p$matrix.info$Mask <- NULL
  }

  x3p
}