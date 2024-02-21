#' Rotate an x3p object
#'
#' Rotate the surface matrix and mask of an x3p object. Also adjust meta information.
#' @param x3p x3p object
#' @param angle rotate counter-clockwise by angle in degrees.
#' @importFrom imager as.cimg pad imrotate
#' @importFrom dplyr near
#' @export
#' @examples
#' \dontrun{
#' logo <- x3p_read(system.file("csafe-logo.x3p", package = "x3ptools"))
#' color_logo <- png::readPNG(system.file("csafe-color.png", package = "x3ptools"))
#' logoplus <- x3p_add_mask(logo, as.raster(color_logo))
#' dim(logoplus$surface.matrix)
#' dim(logoplus$mask)
#' x3p_image(logoplus, multiply = 50, size = c(741, 419), zoom = 0.5)
#'
#' logoplus60 <- x3p_rotate(x3p = logoplus, angle = 60)
#' dim(logoplus60$surface.matrix)
#' dim(logoplus60$mask)
#' x3p_image(logoplus60, multiply = 50, size = c(741, 419), zoom = 0.75)
#' }
x3p_rotate <- function(x3p, angle = 90) {
  stopifnot(is.numeric(angle))
  angle <- angle %% 360
  theta <- angle / 180 * pi
  if (near(angle, 0)) {
    return(x3p)
  }
  x3p_shift <- x3p$surface.matrix
  NA_val <- min(x3p$surface.matrix, na.rm = TRUE) - 0.1 *
    diff(range(x3p$surface.matrix, na.rm = TRUE))
  x3p_shift[is.na(x3p$surface.matrix)] <- NA_val
  x3p_cimg <- as.cimg(x3p_shift)
  x3p_cimg_pad_rotate <- x3p_cimg %>% imrotate(-angle)
  x3p_matrix_pad_rotate <- x3p_cimg_pad_rotate %>% as.matrix()
  x3p_matrix_pad_rotate[near(x3p_matrix_pad_rotate, NA_val)] <- NA
  na_matrix <- x3p_matrix_pad_rotate %>% is.na()
  na_row <- rowSums(na_matrix) == (ncol(na_matrix))
  na_col <- colSums(na_matrix) == (nrow(na_matrix))
  x3p_matrix_pad_rotate <- x3p_matrix_pad_rotate[
    !na_row,
    !na_col
  ]
  x3p_pad_rotate <- x3p
  x3p_pad_rotate$header.info$sizeX <- nrow(x3p_matrix_pad_rotate)
  x3p_pad_rotate$header.info$sizeY <- ncol(x3p_matrix_pad_rotate)
  x3p_pad_rotate$matrix.info$MatrixDimension$SizeX <- list(nrow(x3p_matrix_pad_rotate))
  x3p_pad_rotate$matrix.info$MatrixDimension$SizeY <- list(ncol(x3p_matrix_pad_rotate))
  x3p_pad_rotate$surface.matrix <- x3p_matrix_pad_rotate
  if (!is.null(x3p$mask)) {
    x3p_mask_cimg <- as.cimg(x3p$mask)
    NA_val <- "black"
    x3p_mask_cimg_pad_rotate <- x3p_mask_cimg %>% imrotate(-angle)
    x3p_mask_raster_pad_rotate <- x3p_mask_cimg_pad_rotate %>%
      as.raster()
    x3p_mask_raster_pad_rotate[x3p_mask_raster_pad_rotate ==
      NA_val] <- NA
    x3p_mask_raster_pad_rotate <- x3p_mask_raster_pad_rotate[
      !na_col,
      !na_row
    ] %>% toupper()
    x3p_pad_rotate$mask <- x3p_mask_raster_pad_rotate
  }
  return(x3p_pad_rotate)
}


#' @rdname x3p_rotate
#' @export
rotate_x3p <- x3p_rotate
