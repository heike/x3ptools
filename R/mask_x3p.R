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
#' x3p <- x3p_read(system.file("sample-land.x3p", package="x3ptools"))
#' # x3p file has mask consisting color raster image:
#' x3p$mask[1:5,1:5]
#' \dontrun{
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' color_logo <- png::readPNG(system.file("csafe-color.png", package="x3ptools"))
#' logoplus <- x3p_add_mask(logo, as.raster(color_logo))
#' x3p_image(logoplus, multiply=50, size = c(741, 419),zoom = 0.5)
#' }
x3p_add_mask <- function(x3p, mask = NULL) {
  stopifnot("x3p" %in% class(x3p))
  
  # This is necessary so that mask information can be added
  # HH: I'm not sure how this could happen. Both df_to_x3p and x3p_read create the matrix
  if (!"matrix.info" %in% names(x3p)) {
    x3p$matrix.info <- list(MatrixDimension = list(
      SizeX = dim(x3p$surface.matrix)[1],
      SizeY = dim(x3p$surface.matrix)[2],
      SizeZ = 1
    ))
  }
  
  dims <- rev(dim(x3p$surface.matrix))
  if (is.null(mask)) {
    if (!"Mask" %in% names(x3p$matrix.info)) {
      x3p$matrix.info$Mask <- list(
        Background = if (length(unique(mask)) == 1) {
          list(unique(mask))
        } else {
          list("#cd7f32")
        },
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

#' Add annotations to an x3p object
#'
#' Annotations in an x3p object are  legend entries for each color of a mask.
#' @param x3p x3p object
#' @param color name or hex value of color
#' @param annotation character value describing the region
#' @return x3p object with the added annotations
#' @export
#' @examples 
#' \dontrun{
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' color_logo <- png::readPNG(system.file("csafe-color.png", package="x3ptools"))
#' logoplus <- x3p_add_mask(logo, as.raster(color_logo))
#' x3p_image(logoplus, multiply=50, size = c(741, 419),zoom = 0.5)
#' logoplus <- x3p_add_annotation(logoplus, "#FFFFFFFF", "background")
#' logoplus <- x3p_add_annotation(logoplus, "#818285FF", "text")
#' logoplus <- x3p_add_annotation(logoplus, "#F6BD47FF", "fingerprint")
#' logoplus <- x3p_add_annotation(logoplus, "#D2202FFF", "fingerprint")
#' logoplus <- x3p_add_annotation(logoplus, "#92278FFF", "fingerprint")
#' 
#' x3p_add_legend(logoplus)
#' }
x3p_add_annotation <- function(x3p, color, annotation) {
  if (!("Mask" %in% names(x3p$matrix.info))) {
    x3p$matrix.info$Mask <- list("Annotations")
  }

  len <- length(x3p$matrix.info$Mask$Annotations)
  x3p$matrix.info$Mask$Annotations[[len + 1]] <- list()
  x3p$matrix.info$Mask$Annotations[[len + 1]][[1]] <- annotation
  attr(x3p$matrix.info$Mask$Annotations[[len + 1]], "color") <- color
  x3p
}
