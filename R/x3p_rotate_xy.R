#' Rotate an x3p object
#'
#' Rotate the surface matrix of an x3p object. Also adjust meta information.
#' @param x3p x3p object
#' @param angle rotate clockwise by angle degrees given as 30, 60, 90 degree.
#' @import dplyr
#' @importFrom imager as.cimg pad rotate_xy
#' @importFrom raster raster
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' dim(logo$surface.matrix)
#' \dontrun{
#' x3p_image(logo)
#' }
#' # rotate the image by 60 degrees counter-clockwise:
#' logo60 <- x3p_rotate_xy(logo, 60)
#' dim(logo60$surface.matrix)
#' \dontrun{
#' x3p_image(logo60)
#' }

x3p_rotate_xy <- function(x3p, angle) {
  angle <- angle %% 360

  ### Change to contrast color
  x3p_shift <- x3p$surface.matrix
  NA_val <- -(x3p$surface.matrix %>%
    c() %>%
    summary() %>%
    .[c("Min.", "Max.")] %>%
    abs() %>%
    max() %>%
    ceiling())
  x3p_shift[is.na(x3p$surface.matrix)] <- NA_val

  ### Change to raster
  x3p_raster <- t(x3p_shift) %>%
    raster(xmx = (x3p$header.info$sizeX - 1) * x3p$header.info$incrementX, ymx = (x3p$header.info$sizeY - 1) * x3p$header.info$incrementY)

  ### Change to cimg
  x3p_cimg <- as.cimg(x3p_raster)

  ### Compute diagonal length
  diag_len <- sqrt(nrow(x3p_cimg)^2 + ncol(x3p_cimg)^2)

  ### Pad the original cimg object
  x3p_cimg_pad <- x3p_cimg %>%
    pad(nPix = diag_len, axes = "xy", pos = -1, val = NA_val) %>%
    pad(nPix = diag_len - nrow(x3p_cimg), axes = "x", pos = 1, val = NA_val) %>%
    pad(nPix = diag_len - ncol(x3p_cimg), axes = "y", pos = 1, val = NA_val)

  ### Rotate at padding center
  ### interpolation maintain the original scaling
  x3p_cimg_pad_rotate <- x3p_cimg_pad %>%
    rotate_xy(angle, diag_len, diag_len, interpolation = 0L, boundary_conditions = 1L)

  ### Change cimg object to matrix
  x3p_matrix_pad_rotate <- x3p_cimg_pad_rotate %>%
    as.matrix()

  x3p_matrix_pad_rotate[near(x3p_matrix_pad_rotate, NA_val)] <- NA

  ### Remove extra NA space after padding
  x3p_matrix_pad_rotate <- x3p_matrix_pad_rotate %>%
    as.data.frame() %>%
    filter_all(any_vars(!is.na(.))) %>%
    select_if(~ any(!is.na(.))) %>%
    as.matrix()

  ### Copy x3p object
  x3p_pad_rotate <- x3p %>%
    x3p_delete_mask()
  ### Change details of x3p object
  x3p_pad_rotate$header.info$sizeX <- nrow(x3p_matrix_pad_rotate)
  x3p_pad_rotate$header.info$sizeY <- ncol(x3p_matrix_pad_rotate)
  x3p_pad_rotate$matrix.info$MatrixDimension$SizeX <- nrow(x3p_matrix_pad_rotate)
  x3p_pad_rotate$matrix.info$MatrixDimension$SizeY <- ncol(x3p_matrix_pad_rotate)
  x3p_pad_rotate$surface.matrix <- x3p_matrix_pad_rotate

  x3p_pad_rotate
}

