x3p_rotate_2 <- function(x3p, angle = 90) {
  stopifnot(is.numeric(angle))
  angle <- angle %% 360
  if (near(angle, 0)) {
    return(x3p)
  }
  x3p_shift <- x3p$surface.matrix
  NA_val <- min(x3p$surface.matrix, na.rm = TRUE) - 0.1 *
    diff(range(x3p$surface.matrix, na.rm = TRUE))
  x3p_shift[is.na(x3p$surface.matrix)] <- NA_val
  x3p_cimg <- as.cimg(x3p_shift)
  diag_len <- sqrt(nrow(x3p_cimg)^2 + ncol(x3p_cimg)^2)
  x3p_cimg_pad <- x3p_cimg %>%
    pad(
      nPix = diag_len / 2 * abs(sin(angle / 180 * pi)) - ncol(x3p_cimg) / 2 * (1 - abs(cos(angle / 180 * pi))), axes = "y",
      pos = -1, val = NA_val
    ) %>%
    pad(
      nPix = diag_len / 2 * abs(sin(angle / 180 * pi)) - ncol(x3p_cimg) / 2 * (1 - abs(cos(angle / 180 * pi))), axes = "y",
      pos = 1, val = NA_val
    ) %>%
    pad(
      nPix = diag_len / 2 * abs(cos(angle / 180 * pi)) - nrow(x3p_cimg) / 2 * (1 - abs(sin(angle / 180 * pi))),
      axes = "x", pos = 1, val = NA_val
    ) %>%
    pad(
      nPix = diag_len / 2 * abs(cos(angle / 180 * pi)) - nrow(x3p_cimg) / 2 * (1 - abs(sin(angle / 180 * pi))),
      axes = "x", pos = -1, val = NA_val
    )
  x3p_cimg_pad_rotate <- x3p_cimg_pad %>% rotate_xy(-angle,
    diag_len / 2, diag_len / 2,
    interpolation = 0L, boundary_conditions = 1L
  )
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
    diag_len <- sqrt(nrow(x3p_mask_cimg)^2 + ncol(x3p_mask_cimg)^2)
    NA_val <- "black"
    x3p_mask_cimg_pad <- x3p_mask_cimg %>%
      pad(
        nPix = diag_len / 2 * abs(sin(angle / 180 * pi)) - ncol(x3p_cimg) / 2 * (1 - abs(cos(angle / 180 * pi))), axes = "y",
        pos = -1, val = NA_val
      ) %>%
      pad(
        nPix = diag_len / 2 * abs(sin(angle / 180 * pi)) - ncol(x3p_cimg) / 2 * (1 - abs(cos(angle / 180 * pi))), axes = "y",
        pos = 1, val = NA_val
      ) %>%
      pad(
        nPix = diag_len / 2 * abs(cos(angle / 180 * pi)) - nrow(x3p_cimg) / 2 * (1 - abs(sin(angle / 180 * pi))),
        axes = "x", pos = 1, val = NA_val
      ) %>%
      pad(
        nPix = diag_len / 2 * abs(cos(angle / 180 * pi)) - nrow(x3p_cimg) / 2 * (1 - abs(sin(angle / 180 * pi))),
        axes = "x", pos = -1, val = NA_val
      )
    x3p_mask_cimg_pad_rotate <- x3p_mask_cimg_pad %>% rotate_xy(-angle,
      diag_len / 2, diag_len / 2,
      interpolation = 0L, boundary_conditions = 1L
    )
    x3p_mask_raster_pad_rotate <- x3p_mask_cimg_pad_rotate %>%
      as.raster()
    x3p_mask_raster_pad_rotate[x3p_mask_raster_pad_rotate ==
      "#000000"] <- NA
    x3p_mask_raster_pad_rotate <- x3p_mask_raster_pad_rotate[
      !na_col,
      !na_row
    ] %>% toupper()
    x3p_pad_rotate$mask <- x3p_mask_raster_pad_rotate
  }
  return(x3p_pad_rotate)
}

x3p_vertical_2 <- function(x3p, freqs = c(0, 0.3, 0.7, 1), ntheta = 720, min_score_cut = 0.1,
                           ifplot = FALSE, loess_span = 0.2) {
  assert_that(
    "x3p" %in% class(x3p), is.numeric(freqs), near(
      length(freqs),
      4
    ), near(freqs[1], 0), near(freqs[4], 1), is.count(ntheta),
    is.number(min_score_cut), is.flag(ifplot), is.number(loess_span),
    loess_span > 0
  )
  x3p_bin <- x3p %>% x3p_bin_stripes(
    direction = "vertical",
    colors = c("#b12819", "#ffffff", "#134D6B"), freqs = freqs
  )
  x3p_bin_red <- x3p_extract(x3p_bin, mask_vals = "#b12819")
  x3p_bin_blue <- x3p_extract(x3p_bin, mask_vals = "#134D6B")
  angle_red <- x3p_MLE_angle_vec(x3p_bin_red,
    ntheta = ntheta,
    min_score_cut = min_score_cut, ifplot = ifplot, loess_span = loess_span
  )
  angle_blue <- x3p_MLE_angle_vec(x3p_bin_blue,
    ntheta = ntheta,
    min_score_cut = min_score_cut, ifplot = ifplot, loess_span = loess_span
  )
  angles <- c(angle_red, angle_blue)
  angles <- ifelse(angles > 90, -(180 - angles), angles)
  angle <- mean(angles)
  if (sd(angles) > 10) {
    warning("more than 10 degrees of variation in angles. cause for concern???")
  }
  if (abs(diff(range(angles))) >= 90) {
    warning("more than 90 degrees of range in angles. cause for concern???")
  }
  x3p_bin_rotate <- x3p_rotate_2(x3p_bin, angle = angle)
  if (ifplot) {
    attr(x3p_bin_rotate, "nfline_red_plot") <- attr(
      angle_red,
      "nfline_plot"
    )
    attr(x3p_bin_rotate, "MLE_loess_red_plot") <- attr(
      angle_red,
      "MLE_loess_plot"
    )
    attr(x3p_bin_rotate, "nfline_blue_plot") <- attr(
      angle_blue,
      "nfline_plot"
    )
    attr(x3p_bin_rotate, "MLE_loess_blue_plot") <- attr(
      angle_blue,
      "MLE_loess_plot"
    )
  }
  return(x3p_bin_rotate)
}
