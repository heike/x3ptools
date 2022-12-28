#' Extract profiles with multiple segments
#' 
#' @param x3p object
#' @param width segment width 
#' @param col color
#' @param linewidth integer value specifying the width for the profile
#' @param verbose logical 
#' @return x3p object with added lines
#' @importFrom dplyr mutate left_join select desc add_tally ungroup
#' @importFrom purrr pmap map map_dbl pmap_df 
#' @examples 
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' logo <- x3p_m_to_mum(logo)
#' if(interactive())
#'   x3p_extract_profile_segments(logo, 850, col="#ffffff")
x3p_extract_profile_segments <- function(x3p, width, col, linewidth, verbose = TRUE) {
  # pass R CMD CHECK
  x <- y <- height <- value <- orig_x <- orig_y <- piece <- NULL
  mask.x <- mask.y <- NULL
  # how many pieces do we need assuming we use 10% for overlap?
  dims <- dim(x3p$surface.matrix)
  w10 <- round(.1*width)
  w90 <- width - w10
  orig_scale <- x3p$header.info$incrementY
  x3p$header.info$incrementY <- 1
  x3p$header.info$incrementX <- 1
  
  if (verbose) {
    message(sprintf("Setting up %d pieces ...", dims[1] %/% w90 + 1))
  }
  
  dframe <- data.frame(x = seq(1, dims[1], by = w90), 
                       y = 1, width = width, height=dims[2]-1)
  dframe <- dframe %>% mutate(
    width = ifelse(x+width > dims[1], dims[1]-x, width)
  )
  
  dframe <- dframe %>% mutate(
    x3p = purrr::pmap(list(x, y, width, height), 
                      .f = function(x, y, width, height) {
                        x3p_crop(x3p, x,y,width,height)})
  )
  if (verbose) {
    message("done\nExtract profiles for each piece ...\n")
  }
  dframe <- dframe %>% mutate(
    x3p = x3p %>% purrr::map(.f = function(x) {
      x %>% x3ptools::x3p_image()
      x <- x %>% x3p_extract_profile(linewidth=5)
    })
  )
  if (verbose) {
    message("done\nCombine profiles into one ...\n")
  }
  dframe <- dframe %>% mutate(
    first10 = x3p %>% purrr::map_dbl(.f = function(x) {
      x$line_df %>% filter(x <= w10) %>% summarize(value = mean(value, na.rm=TRUE)) %>% pull(1)
    }),
    last10 = x3p %>% purrr::map_dbl(.f = function(x) {
      x$line_df %>% filter(x > w90) %>% summarize(value = mean(value, na.rm=TRUE)) %>% pull(1)
    })
  )
  idx <-1:nrow(dframe)
  dframe$y_adjust <- 0
  if (length(idx) > 1) {
  idx_last <- c(NA, 1:(nrow(dframe)-1))
  
  dframe$y_adjust <- dframe$last10[idx_last]-dframe$first10[idx]
  dframe$y_adjust[1] <- 0
  dframe$y_adjust <- cumsum(dframe$y_adjust)
  
  }
  
  lines <- purrr::pmap_df(list(dframe$x3p, dframe$x, dframe$y_adjust),
                          .f = function(x3p, offset, adjust) {
                            x3p$line_df %>% mutate(orig_x = orig_x + offset, value = value+adjust)
                          }, .id="piece")
  
  lines$mask <- col
  if (is.null(x3p$mask)) x3p <- x3p %>% x3p_add_mask()
  x3p_df <- x3p %>% x3p_to_df()
  lines <- lines %>% dplyr::select(-x) %>% 
    rename(y = orig_y, x = orig_x)
  if (verbose) {
    message("done\nIncorporate into mask ...\n")
  }
  
  x3p_df <- x3p_df %>% left_join(
    lines %>% dplyr::select(-value, -piece) %>% unique(), by=c("x", "y"))
  x3p_df <- x3p_df %>% mutate(
    mask = ifelse(is.na(mask.y), mask.x, mask.y)
  ) %>% dplyr::select(-mask.x, -mask.y)
  
  if (verbose) {
    message("done")
  }
  x3p_df <- x3p_df %>% arrange(x,desc(y))
  mask <- matrix(x3p_df$mask, nrow = dims[2])
  
  x3p$mask <- as.raster(mask)
  x3p$header.info$incrementY <- orig_scale
  x3p$header.info$incrementX <- orig_scale
  x3p$line_df <- lines %>% mutate(x = x * orig_scale, y = y * orig_scale)
  x3p
}