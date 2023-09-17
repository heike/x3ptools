#' Rotate an x3p object
#'
#' Rotate the surface matrix and mask of an x3p object. Also adjust meta information.
#' @param x3p x3p object
#' @param angle rotate counter-clockwise by angle degrees given as 30, 60, 90 degree.
#' @import dplyr
#' @importFrom imager as.cimg pad rotate_xy
#' @importFrom raster raster as.matrix
#' @export
#' @examples
#' \dontrun{
#' logo <- x3p_read(system.file("csafe-logo.x3p", package = "x3ptools"))
#' color_logo <- png::readPNG(system.file("csafe-color.png", package="x3ptools"))
#' logoplus <- x3p_add_mask(logo, as.raster(color_logo))
#' dim(logoplus$surface.matrix)
#' dim(logoplus$mask)
#' x3p_image(logoplus, multiply=50, size = c(741, 419),zoom = 0.5)
#'
#' logoplus60 <- x3p_rotate(x3p = logoplus, angle = 60)
#' dim(logoplus60$surface.matrix)
#' dim(logoplus60$mask)
#' x3p_image(logoplus60, multiply=50, size = c(741, 419),zoom = 0.75)
#' }

x3p_rotate <- function(x3p, angle = 90) {
  if (identical(angle %% 360, 0)) {
    return(x3p)
  } else {
    angle <- angle %% 360
  }

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
    rotate_xy(-angle, diag_len, diag_len, interpolation = 0L, boundary_conditions = 1L)

  ### Change cimg object to matrix
  x3p_matrix_pad_rotate <- x3p_cimg_pad_rotate %>%
    as.matrix()

  x3p_matrix_pad_rotate[near(x3p_matrix_pad_rotate, NA_val)] <- NA

  ### Remove extra NA space after padding
  na_matrix <- x3p_matrix_pad_rotate %>%
    is.na()
  na_row <- rowSums(na_matrix) == (ncol(na_matrix))
  na_col <- colSums(na_matrix) == (nrow(na_matrix))
  x3p_matrix_pad_rotate <- x3p_matrix_pad_rotate[!na_row, !na_col]

  ### Copy x3p object
  x3p_pad_rotate <- x3p
  ### Change details of x3p object
  x3p_pad_rotate$header.info$sizeX <- nrow(x3p_matrix_pad_rotate)
  x3p_pad_rotate$header.info$sizeY <- ncol(x3p_matrix_pad_rotate)
  x3p_pad_rotate$matrix.info$MatrixDimension$SizeX <- list(nrow(x3p_matrix_pad_rotate))
  x3p_pad_rotate$matrix.info$MatrixDimension$SizeY <- list(ncol(x3p_matrix_pad_rotate))
  x3p_pad_rotate$surface.matrix <- x3p_matrix_pad_rotate

  if (!is.null(x3p$mask)) {
    ### Change to cimg
    x3p_mask_cimg <- x3p$mask %>%
      raster::as.matrix() %>%
      t() %>%
      as.raster() %>%
      as.cimg()

    ### Compute diagonal length
    diag_len <- sqrt(nrow(x3p_mask_cimg)^2 + ncol(x3p_mask_cimg)^2)

    ### Pad the original cimg object
    NA_val <- "black"
    x3p_mask_cimg_pad <- x3p_mask_cimg %>%
      pad(nPix = diag_len, axes = "xy", pos = -1, val = NA_val) %>%
      pad(nPix = diag_len - nrow(x3p_mask_cimg), axes = "x", pos = 1, val = NA_val) %>%
      pad(nPix = diag_len - ncol(x3p_mask_cimg), axes = "y", pos = 1, val = NA_val)

    ### Rotate at padding center
    ### interpolation maintain the original scaling
    x3p_mask_cimg_pad_rotate <- x3p_mask_cimg_pad %>%
      rotate_xy(angle, diag_len, diag_len, interpolation = 0L, boundary_conditions = 1L)

    ### Change cimg object to raster
    x3p_mask_raster_pad_rotate <- x3p_mask_cimg_pad_rotate %>%
      as.raster()

    x3p_mask_raster_pad_rotate[x3p_mask_raster_pad_rotate == "#000000"] <- NA

    ### Remove extra NA space after padding
    x3p_mask_raster_pad_rotate <- (x3p_mask_raster_pad_rotate %>%
      as.matrix())[!na_row, !na_col] %>%
      t() %>%
      as.raster() %>%
      toupper()

    # x3p_add_mask(x3p_pad_rotate, x3p_mask_raster_pad_rotate)
    x3p_pad_rotate$mask <- x3p_mask_raster_pad_rotate
  }

  return(x3p_pad_rotate)
}


#' @rdname x3p_rotate
#' @export
rotate_x3p <- x3p_rotate
