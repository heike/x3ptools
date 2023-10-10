#' Extract profiles from surface using multiple segments
#' 
#' The 3d topographic surface is split into multiple segments of width `width` (in pixels)
#' using an overlap of 10% between segments. For each segment, a line is extracted (with `x3p_extract_profile`).
#' Line segments are projected onto the mask of the initial x3p object and exported as a `lines` attribute. 
#' @param x3p object
#' @param width segment width 
#' @param col color
#' @param linewidth integer value specifying the width for the profile
#' @param verbose logical 
#' @return x3p object with added `lines` attribute.
#' @importFrom dplyr mutate left_join select desc add_tally ungroup n
#' @importFrom purrr pmap map map_dbl pmap_df 
#' @importFrom tidyr unnest
#' @export
#' @examples 
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' logo <- x3p_m_to_mum(logo)
#' if(interactive())
#'   x3p_extract_profile_segments(logo, 850, col="#ffffff", linewidth=5)
x3p_extract_profile_segments <- function(x3p, width, col="#FF0000", linewidth=11, verbose = TRUE) {
  # pass R CMD CHECK
  x <- y <- height <- value <- orig_x <- orig_y <- piece <- NULL
  mask.x <- mask.y <- line <- offset_x <- value_adjust <- NULL
  offset_y <- NULL
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
      x <- x %>% x3p_extract_profile(linewidth=linewidth)
    })
  )
 
  if (verbose) {
    message("done\nIncorporate into mask ...\n")
  }
  
  dframe <- dframe %>% mutate(
    line = x3p %>% purrr::map(.f = function(x) x$line)
  )
  
  masklines <- dframe %>% select(-x3p) %>% 
    rename(offset_x = x, offset_y = y) %>%
    mutate(piece = 1:n()) %>%
    tidyr::unnest(col=line)
  
  masklines$mask <- col
  masklines <- masklines %>% mutate(
    x = round(orig_x + offset_x),
    y = round(orig_y + offset_y)
  ) %>% select(x, y, mask) %>% 
    unique()

  if (is.null(x3p$mask)) x3p <- x3p %>% x3p_add_mask()
  
  x3p_df <- x3p %>% x3p_to_df()
 # masklines %>% anti_join(x3p_df, by=c("x", "y")) 
  
  x3p_df <- x3p_df %>% left_join(masklines, by=c("x", "y"))
  x3p_df <- x3p_df %>% mutate(
    mask = ifelse(is.na(mask.y), mask.x, mask.y)
  ) %>% select(-mask.x, -mask.y)
  
  if (verbose) {
    message("done\nCombine profiles into one ...\n")
  }

  # check the `value` values of overlapping pieces and adjust consecutive pieces for any systematic 
  # differences in `value` 
   dframe <- dframe %>% mutate(
     value_first10 = x3p %>% purrr::map_dbl(.f = function(x) {
       x$line %>% filter(x <= w10) %>% summarize(value = mean(value, na.rm=TRUE)) %>% pull(1)
     }),
     value_last10 = x3p %>% purrr::map_dbl(.f = function(x) {
       x$line %>% filter(x > w90) %>% summarize(value = mean(value, na.rm=TRUE)) %>% pull(1)
     })
   )
   idx <-1:nrow(dframe)
   dframe$value_adjust <- 0
   if (length(idx) > 1) {
     idx_last <- c(NA, 1:(nrow(dframe)-1))
  
     dframe$value_adjust <- dframe$value_last10[idx_last]-dframe$value_first10[idx]
     dframe$value_adjust[1] <- 0
     dframe$value_adjust <- cumsum(dframe$value_adjust)
   }
   lines <- dframe %>% select(-x3p) %>% 
     rename(offset_x = x, offset_y = y) %>%
     mutate(piece = 1:n()) %>%
     tidyr::unnest(col=line) %>%
     mutate(
       value = value + value_adjust,
       x = orig_x+offset_x
       )
   # lines %>% ggplot(aes(x = orig_x+offset_x, y = value)) + geom_point(aes(colour=factor(piece)))
   
  
  if (verbose) {
    message("done")
  }
  x3p_df <- x3p_df %>% arrange(x,desc(y))
  mask <- matrix(x3p_df$mask, nrow = dims[2])
  
  x3p$mask <- as.raster(mask)
  x3p$header.info$incrementY <- orig_scale
  x3p$header.info$incrementX <- orig_scale
  
  x3p$lines <- lines %>% mutate(
    x = x * orig_scale, 
    y = y * orig_scale,
    orig_x = orig_x+offset_x,
    orig_y = orig_y+offset_y) %>%
    select(x, y, value, orig_x, orig_y, piece)
  x3p
}