#' Rotate an x3p object
#'
#' Rotate the surface matrix and mask of an x3p object. Also adjust meta information.
#' @param x3p x3p object
#' @param angle rotate counter-clockwise by angle in degrees.
#' @importFrom imager cimg as.cimg pad imrotate
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
  if (near(angle, 0)) {
    return(x3p)
  }
  not_zero <- 10
  shift_up <- not_zero + abs(min(x3p$surface.matrix, na.rm = TRUE))
  x3p_shift <- x3p$surface.matrix + shift_up
  NA_val <- 0

  x3p_shift[is.na(x3p$surface.matrix)] <- NA_val
  x3p_cimg <- as.cimg(x3p_shift)

  x3p_cimg_rotate <- x3p_cimg %>%
    imrotate(-angle, interpolation = 0L)
  x3p_matrix_rotate <- x3p_cimg_rotate %>%
    as.matrix()
  x3p_matrix_rotate[x3p_matrix_rotate < shift_up / 2] <- NA # there should not be any values between 0 and shift_up
  x3p_matrix_rotate <- x3p_matrix_rotate - shift_up

  na_matrix <- x3p_matrix_rotate %>% is.na()
  na_row <- rowSums(na_matrix) == (ncol(na_matrix))
  na_col <- colSums(na_matrix) == (nrow(na_matrix))
  x3p_matrix_rotate <- x3p_matrix_rotate[
    !na_row,
    !na_col
  ]
  x3p_rotate <- x3p
  x3p_rotate$header.info$sizeX <- nrow(x3p_matrix_rotate)
  x3p_rotate$header.info$sizeY <- ncol(x3p_matrix_rotate)
  x3p_rotate$matrix.info$MatrixDimension$SizeX <- list(nrow(x3p_matrix_rotate))
  x3p_rotate$matrix.info$MatrixDimension$SizeY <- list(ncol(x3p_matrix_rotate))
  x3p_rotate$surface.matrix <- x3p_matrix_rotate
  if (!is.null(x3p$mask)) {
    x3p_mask_cimg <- as.cimg(x3p$mask)
    x3p_mask_cimg_rotate <- x3p_mask_cimg %>%
      imrotate(-angle, interpolation = 0L)
    x3p_mask_raster_rotate <- x3p_mask_cimg_rotate %>%
      as.raster()
    x3p_mask_raster_rotate[is.na(x3p_matrix_rotate)] <- NA
    x3p_mask_raster_rotate <- x3p_mask_raster_rotate[
      !na_col,
      !na_row
    ] %>% toupper()
    x3p_rotate$mask <- x3p_mask_raster_rotate
  }
  return(x3p_rotate)
}


#' @rdname x3p_rotate
#' @export
rotate_x3p <- x3p_rotate
