#' Average an x3p object
#' 
#' Calculate blockwise summary statistics on the surface matrix of an x3p.
#' If the x3p object has a mask, the mode of the mask value
#' @param x3p x3p object
#' @param b positive integer value, block size
#' @param f function aggregate function
#' @param ... parameters passed on to function f. Make sure to use na.rm = T as needed.
#' @export
#' @importFrom dplyr group_by summarize ungroup add_tally
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' small <- x3p_average(logo)
x3p_average <- function(x3p, b = 10, f = mean, ...) {
  stopifnot("x3p" %in% class(x3p))
  x <- y <- value <- NULL # pass R CMD CHECK
  mask <- mask_n <- NULL
    
  df <- x3p_to_df(x3p)
  scale <- x3p_get_scale(x3p)
  df$x <- round(df$x/scale,0)
  df$y <- round(df$y/scale,0)
  # round x and y to block size
  df$x <- df$x %/% b
  df$y <- df$y %/% b
#  browser()
  if(!is.null(x3p$mask)) {
    df <- ungroup(add_tally(group_by(df, x, y, mask), name="mask_n"))

    df <- summarize(group_by(df, x, y),
                    value = f(value, ...),
                    mask = mask[which.max(mask_n)] # in case of ties, take the first
    )
    # if(!is.null(x3p$matrix.info$Mask$Annotations)) {
    #   browser()
    #   colors <- x3p_mask_legend(x3p)
    #   annotations <- data.frame(
    #     mask_low = tolower(colors),
    #     annotation = names(colors)
    #     )
    #   df <- left_join(
    #       mutate(df, mask_low=tolower(mask)),
    #       annotations,
    #       by="mask_low"
    #     )
    #   df <- select(df, -mask_low)
    # }
    
  } else {
    df <- summarize(group_by(df, x, y),
                    value = f(value, ...)
    )
  }  
  
  # scale x and y back
  df$x <- df$x * scale * b
  df$y <- df$y * scale * b
  df_to_x3p(df)
}


