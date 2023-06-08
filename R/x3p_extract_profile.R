#' Interactively select a line on the active rgl device 
#' 
#' In the active rgl device select a line on the 3d surface by clicking on start and end-point (order matters). These points define the beginning and end  of a line segment.
#' The line segment is drawn on the mask of the x3p object. 
#' The line object is returned as part of the x3p object, if `line_result` is set to `TRUE`
#' @param x3p x3p file
#' @param col character value of the selection color
#' @param update boolean value, whether the rgl window should be updated to show the selected circle
#' @param line_result  enhance result by a data frame of the line: NULL for no, "raw" for data frame of original x and y (in the mask) and
#' projected x onto the line, "equi-spaced" (default) returns a data frame with equispaced x values after fitting a loess smooth to the raw values. 
#' Note that variable x indicates the direction from first click (x=0) to 
#' the second click (max x). 
#' @param multiply integer value, factor to multiply surface values.  Only applied if update is true. Defaults to 5, 
#' @param linewidth line width of the extracted line. Defaults to 1.
#' @return x3p file with identified line in the mask. Depending on the setting of `line_result`
#' additional information on the line is attached as a data frame. 
#' @export
#' @importFrom dplyr arrange between filter mutate rename
#' @examples 
#' \dontrun{
#' if (interactive) {
#'   x3p <- x3p_read(system.file("sample-land.x3p", package="x3ptools"))
#'   x3p %>% image_x3p(size=dim(x3p$surface.matrix), multiply=1, zoom=.3)
#'   x3p <- x3p_extract_profile(x3p, update=TRUE, col="#FFFFFF") 
#'   x3p$line_df %>% 
#'     ggplot(aes(x = x, y = value)) + geom_line() 
#'  
#'  x3p$line_df$y <- 1      
#'  sigs <- bulletxtrctr::cc_get_signature(ccdata = x3p$line_df, 
#'    grooves = list(groove=range(x3p$line_df$x)), span1 = 0.75, span2 = 0.03)
#'  sigs %>% 
#'    ggplot(aes(x = x)) + 
#'      geom_line(aes(y = raw_sig), colour = "grey50") +
#'      geom_line(aes(y = sig), size = 1) +
#'      theme_bw() 
#' }}
x3p_extract_profile <- function(x3p, col = "#FF0000", update=TRUE, line_result= "equi-spaced", multiply = 5, linewidth = 1) {
  cat("Select start point and endpoint on the surface ...\n")
  stopifnot("x3p" %in% class(x3p))
  ids <- rgl::ids3d()
  if (nrow(ids) == 0) {
    size <- dim(x3p$surface.matrix)
    size[2] <- round(500/size[1]*size[2])
    size[1] <- 500
    x3p %>% x3p_image(size = size, zoom=0.8)
  }
  rgl::rgl.bringtotop()
  
  # initialize values
  orig_x <- orig_y <- `p-x.m` <- `p-x.n` <- value <- NULL
  
  multiply <- multiply
  surface <- x3p$surface.matrix
  z <- multiply * surface
  yidx <- ncol(z):1
  y <- x3p$header.info$incrementY * yidx
  x <- x3p$header.info$incrementX * (1:nrow(x3p$surface.matrix))
  
  xidx <- rep(x, length(y))
  yidx <- rep(y, each=length(x))
  selected <-  identify3d(xidx, yidx ,z, n=2, buttons=c("left", "middle"))
  #  browser()
  
  coord1 <- xidx[selected]
  coord2 <- yidx[selected]
  
  X <- c(coord1[1], coord2[1])
  Y <- c(coord1[2], coord2[2])
  
  m <- Y-X 
  dm <- sqrt(sum(m^2)) # length of Y-X
  m <- m/dm
  
  n <- c(m[2],-m[1]) # normal vector to m
  
  if (is.null(x3p$mask)) x3p <- x3p %>% x3p_add_mask()
  #  
  x3p_df <- x3p_to_df(x3p)
  x3p_df <- x3p_df %>% mutate(
    `p-x.n` = (x - X[1])*n[1] + (y - X[2])*n[2],
    `p-x.m` = (x - X[1])*m[1] + (y - X[2])*m[2]
  )
  eps <- x3p %>% x3p_get_scale() * linewidth/2
  on_line <- abs(x3p_df$`p-x.n`)< eps & dplyr::between(x3p_df$`p-x.m`, 0, dm)
  x3p_df$mask[on_line] <- col
  tmp <- x3p_df %>% df_to_x3p()
  #  if (is.null(x3p$mask)) x3p <- x3p %>% x3p_add_mask()
  #  browser()
  x3p$mask <- tmp$mask
  
  #  browser()
  if (update) {
    rgl::rgl.bringtotop()
    clear3d() # remove the active scene
    surface3d(x, y, z, col = as.vector(x3p$mask), back = "fill")
    #  x3p %>% image_x3p(update=TRUE)
  }
  
  if (!is.null(line_result)) {
    line_df <- x3p_df %>% 
      filter(abs(`p-x.n`)<eps, between(`p-x.m`, 0, dm)) %>%
      rename(
        orig_x = x,
        orig_y = y,
        x = `p-x.m`,
        y = `p-x.n`
      ) %>%
      select(x, y, orig_x, orig_y, value) %>%
      arrange(x)
    
    if (line_result == "raw")  {
      line <- line_df
    } 
    if (line_result == "equi-spaced") {
      
      #ps <- sequence_points(0, Y-X, by=x3p %>% x3p_get_scale())
      scale <- x3p %>% x3p_get_scale()
      line_df <- line_df %>% mutate(
        x = round(x/scale),
        y = round(y/scale)
      ) 
      line <- line_df %>% group_by(x) %>% 
        summarize(
          value = median(value, na.rm=TRUE),
          orig_x = median(orig_x),
          orig_y = median(orig_y),
          y = 0,
          n = n()) %>% 
        mutate(x = x*scale)
    }
    attributes(line)$click <- list(start=X, end=Y)
    
    x3p$line <- line
  }
  
  x3p
}