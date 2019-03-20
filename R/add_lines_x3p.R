scale_to_pix <- function(x3p, which, values) {
  scale <- 1
  if (which == "x") {
    scale <- x3p$header.info$incrementX
    return(round(values / scale + 1, 0))
  }
  if (which == "y") {
    scale <- x3p$header.info$incrementY
    
    return(x3p$header.info$sizeY - round(values / scale + 1, 0))
  }
}

#' Add vertical line to the mask of an x3p object
#'
#' Add vertical lines to overlay the surface of an x3p object. Lines are added to a mask. In case no mask exists, one is created.
#' @param x3p x3p object
#' @param xintercept (vector of) numerical values for the position of the lines.
#' @param size width (in pixels) of the line
#' @param color (vector of) character values to describe color of lines
#' @return x3p object with added vertical lines in the mask
#' @export
#' @examples
#' \dontrun{
#' logo <- read_x3p(system.file("csafe-logo.x3p", package = "x3ptools"))
#' logo_color <- magick::image_read(system.file("csafe-color.png", package = "x3ptools"))
#' logoplus <- x3p_add_mask(logo, as.raster(logo_color))
#' # ten vertical lines across:
#' logoplus <- x3p_add_vline(logo, seq(0, 740 * 6.4500e-7, length = 5), size = 3)
#' image_x3p(logoplus, size = c(741, 419), zoom = 0.5)
#' }
x3p_add_vline <- function(x3p, xintercept, size = 5, color = "#e6bf98") {
  stopifnot("x3p" %in% class(x3p))
  if (!exists("mask", x3p)) x3p <- x3p_add_mask(x3p)
  
  xs <- scale_to_pix(x3p, "x", xintercept)
  
  xindex <- rep(xs, each = size) + 1:size - 1 - size %/% 2
  colors <- rep(color, each = size, length = length(xindex))
  valid <- (xindex > 0) & (xindex <= x3p$header.info$sizeX)
  #  browser()
  x3p$mask[, xindex[valid]] <- colors[valid]
  
  x3p
}





#' Add horizontal line to the mask of an x3p object
#'
#' Add horizontal lines to overlay the surface of an x3p object. Lines are added to a mask. In case no mask exists, one is created.
#' @param x3p x3p object
#' @param yintercept (vector of) numerical values for the position of the lines.
#' @param size width (in pixels) of the line
#' @param color (vector of) character values to describe color of lines
#' @return x3p object with added vertical lines in the mask
#' @export
#' @examples
#' \dontrun{
#' logo <- read_x3p(system.file("csafe-logo.x3p", package = "x3ptools"))
#' color_logo <- magick::image_read(system.file("csafe-color.png", package = "x3ptools"))
#' logoplus <- x3p_add_mask(logo, as.raster(color_logo))
#' # five horizontal lines at equal intervals:
#' logoplus <- x3p_add_hline(logo, seq(0, 418 * 6.4500e-7, length = 5), size = 3)
#' image_x3p(logoplus, size = c(741, 419), zoom = 0.5)
#' }
x3p_add_hline <- function(x3p, yintercept, size = 5, color = "#e6bf98") {
  stopifnot("x3p" %in% class(x3p))
  if (!exists("mask", x3p)) x3p <- x3p_add_mask(x3p)
  
  ys <- scale_to_pix(x3p, "y", yintercept)
  
  yindex <- rep(ys, each = size) + 1:size - 1 - size %/% 2
  colors <- rep(color, each = size, length = length(yindex))
  valid <- (yindex > 0) & (yindex <= x3p$header.info$sizeY)
  #  browser()
  x3p$mask[yindex[valid], ] <- colors[valid]
  
  x3p
}


#' Calculate grid spacing
#' 
#' Helper function, not exported.
#' @param x3p x3p file
#' @param spaces space between grid lines
#' @param axis axis to calculate, as character
#' @return vector of line locations
calculate_spacing <- function(x3p, spaces, axis = "y") {
  increment <- x3p_show_xml(x3p, paste0("increment", toupper(axis)))[[1]]
  size <- x3p_show_xml(x3p, paste0("size", toupper(axis)))[[1]]
  
  lines <- seq(from = increment, to = size, by = spaces)
}


#' Add a grid of helper lines to the mask of an x3p object
#'
#' Add a grid of lines to overlay the surface of an x3p object.
#' Lines are added to a mask. In case no mask exists, one is created.
#' @param x3p x3p object
#' @param spaces space between grid lines, doubled for x
#' @param size width (in pixels) of the lines
#' @param color (vector of) character values to describe color of lines
#' @return x3p object with added vertical lines in the mask
#' @export
#' @examples
#' \dontrun{
#' logo <- read_x3p(system.file("csafe-logo.x3p", package = "x3ptools"))
#' # ten vertical lines across:
#' logoplus <- x3p_add_grid(logo,
#'   spaces = 50e-6, size = c(1, 3, 5),
#'   color = c("grey50", "black", "darkred")
#' )
#' image_x3p(logoplus, size = c(741, 419), zoom = 0.5)
#' }
x3p_add_grid <- function(x3p, spaces, size = c(1, 3, 5),
                         color = c("grey50", "black", "darkred")) {
  stopifnot("x3p" %in% class(x3p))
  if (!exists("mask", x3p)) x3p <- x3p_add_mask(x3p)
  
  # ensure that both size and color are the same length
  if (length(color) < length(size)) color <- rep(color, length(size))
  if (length(color) > length(size)) size <- rep(size, length(color))
  
  maxX <- x3p$header.info$sizeX
  maxY <- x3p$header.info$sizeY
  yintercepts <- calculate_spacing(x3p, spaces, "Y")
  xintercepts <- calculate_spacing(x3p, spaces * 2, "X")
  
  if (length(yintercepts) == 1 | length(xintercepts) == 1) {
    warning(paste(
      "Line spacing does not map to x3p file correctly.",
      sprintf("Spaces are at %f x %f, scan has size of %e x %e. ",
              2 * spaces, spaces, maxX, maxY),
      "Remember to specify grid spacing in measured units, not pixels."
    ))
  }
  
  x3plus <- x3p
  
  m <- 1
  for (i in 1:length(size)) {
    if (i > 1 & i %% 2 == 0) { # take every fifth value
      m <- m * 5
    }
    if (i > 1 & i %% 2 == 1) { # take every second value
      m <- m * 2
    }
    xidx <- seq.int(1, length(xintercepts), by = m)
    yidx <- seq.int(1, length(yintercepts), by = m)
    
    x3plus <- x3p_add_vline(x3plus, xintercept = xintercepts[xidx], size = size[i], color = color[i])
    x3plus <- x3p_add_hline(x3plus, yintercept = yintercepts[yidx], size = size[i], color = color[i])
  }
  
  x3plus
}
