#' Draw rectangle on the mask of an x3p file using rgl
#' 
#' Interactive selection of rectangular area on the mask of an x3p object. Once the function runs, the active rgl window is brought to the front.
#' Select the window with a click, then use click & drag to select a rectangular area. On release, this area is marked in the mask and (if update is TRUE) appears in the selection color in the active rgl window.
#' @param x3p x3p file
#' @param col character value of the selection color
#' @param update boolean value, whether the rgl window should be updated to show the selected rectangle
#' @return x3p file with selection in mask
#' @export
#' @examples 
#' \dontrun{
#' if (interactive) {
#'   if (!file.exists("fadul1-1.x3p")) {
#'     url <- "https://tsapps.nist.gov/NRBTD/Studies/CartridgeMeasurement/DownloadMeasurement"
#'     file <- "2d9cc51f-6f66-40a0-973a-a9292dbee36d"
#'     download.file(file.path(url, file), destfile="fadul1-1.x3p")
#'   }
#'   x3p <- x3p_read("fadul1-1.x3p")
#'   x3p_image(x3p, size=c(500,500), zoom=.8)
#'   x3p <- x3p_select(x3p, update=TRUE, col="#FF0000") 
#' 
#'   logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#'   x3p_image(logo, size=c(500,500), zoom = 1)
#'   x3p_select(logo, update=TRUE, col="#00FF00") 
#' }}
x3p_select <- function(x3p, col = "#FF0000", update=TRUE) {
 #   browser()
  stopifnot("x3p" %in% class(x3p))
  ids <- rgl::ids3d()
  if (nrow(ids) == 0) {
    size <- dim(x3p$surface.matrix)
    size[2] <- round(500/size[1]*size[2])
    size[1] <- 500
    x3p %>% x3p_image(size = size, zoom=0.8)
  }
  rgl::rgl.bringtotop()
  cat("select rectangular area (by click and drag) or click to stop\n")
  
  f <- select3d()
  
  multiply <- 5
  surface <- x3p$surface.matrix
  z <- multiply * surface
  yidx <- ncol(z):1
  y <- x3p$header.info$incrementY * yidx
  x <- x3p$header.info$incrementX * (1:nrow(x3p$surface.matrix))
  
  selected <-  f(rep(x, length(y)), rep(y, each=length(x)) ,z)
  
  if (is.null(x3p$mask)) x3p <- x3p %>% x3p_add_mask()
  #  
  
  x3p$mask[matrix(selected, nrow = dim(x3p$mask)[1], byrow=TRUE)] <- col
  if (update) {
 #   browser()
    pop3d() # remove the active scene
    surface3d(x, y, z, col = as.vector(x3p$mask), back = "fill")
  #  x3p %>% image_x3p(update=TRUE)
  }
  x3p
}


#' Interactive selection of region of interest
#' 
#' 
#' @param x3p x3p file
#' @param col character value of the selection color
#' @param mad scalar
#' @param type only "plane" is implemented at the moment
#' @param update boolean value, whether the rgl window should be updated to show the selected rectangle
#' @return x3p file with updated mask
#' @importFrom stats predict
#' @importFrom MASS rlm
#' @export
#' @examples 
#' \dontrun{
#' if (interactive) {
#'   if (!file.exists("fadul1-1.x3p")) {
#'     url <- "https://tsapps.nist.gov/NRBTD/Studies/CartridgeMeasurement/DownloadMeasurement"
#'     file <- "2d9cc51f-6f66-40a0-973a-a9292dbee36d"
#'     download.file(file.path(url, file), destfile="fadul1-1.x3p")
#'   }
#'   x3p <- x3p_read("fadul1-1.x3p")
#'   x3p_image(x3p, size=c(500,500), zoom=.8)
#'   x3p <- x3p_fuzzyselect(x3p, update=TRUE, col="#FF0000") 
#' 
#'   logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#'   x3p_image(logo, size=c(500,500), zoom = 1)
#'   x3p_fuzzyselect(logo, update=TRUE, col="#00FF00") 
#' }}
x3p_fuzzyselect <- function(x3p, col="#FF0000", mad=5, type="plane", update=TRUE) {
  ids <- rgl::ids3d()
  if (nrow(ids) == 0) {
    size <- dim(x3p$surface.matrix)
    size[2] <- round(500/size[1]*size[2])
    size[1] <- 500
    x3p_image(x3p, size = size, zoom=0.8)
  }
  rgl::rgl.bringtotop()
  
  mask <- NULL 

  stop <- FALSE
  cat("select rectangular area or click to stop\n")
  
  while(!stop) {
    tab1 <- table(as.vector(x3p$mask))
    x3p <- x3p_select(x3p, col=col)
    
    tab2 <- table(as.vector(x3p$mask))
    stop <- identical(tab1, tab2)
    
    if (stop) {
      cat("exiting selection.")
      
      return(x3p)
    }
    
    x3p_df <- x3p_to_df(x3p)
    # x3p_df %>% count(mask)
    
    if (type== "plane") {
      filtered_region <- dplyr::filter(x3p_df, mask == col)
      m1 <-   MASS::rlm(value ~ x + y, data = filtered_region)
      
      
      cat("... adding similar cases \n")
      
      x3p_df$p1 <- predict(m1, newdata = x3p_df)
      x3p_df$r1 <- abs(x3p_df$value - x3p_df$p1) 
      
      #x3p_df <- x3p_df %>% filter(r1 < 1*max(abs(m1$residuals)))
      
      x3p_df$mask[which(x3p_df$r1 < mad*max(.Machine$double.eps, mad(m1$residuals)))] = col
    }
    if (type == "distance") {
      # idea: color in all pixels 
    }
    # convert back to x3p
    x3p <- df_to_x3p(x3p_df)
    
    if (update) {
      rgl::pop3d()
      #   browser()
      surface <- x3p$surface.matrix
      z <- 5 * surface
      yidx <- ncol(z):1
      y <- x3p$header.info$incrementY * yidx
      x <- x3p$header.info$incrementX * (1:nrow(x3p$surface.matrix))
      rgl::surface3d(x, y, z, col = x3p_df$mask, back = "fill")
    }
    cat("select a rectangular area on the active rgl device or click on white space to stop ...\n")
    
  }
  
  cat("exiting selection.")
  x3p
}


#' Select a circle area on the surface of an x3p file using rgl
#' 
#' In the active rgl window select a circle on the scan's surface by clicking on three points along the circumference.
#' Make sure that x3p file and the rgl window match. If no rgl window is active, an rgl window opens with the scan. 
#' @param x3p x3p file
#' @param col character value of the selection color
#' @param update boolean value, whether the rgl window should be updated to show the selected circle
#' @return x3p file with selected circle in mask
#' @export
#' @importFrom pracma circlefit
#' @examples 
#' \dontrun{
#' if (interactive) {
#'   if (!file.exists("fadul1-1.x3p")) {
#'     url <- "https://tsapps.nist.gov/NRBTD/Studies/CartridgeMeasurement/DownloadMeasurement"
#'     file <- "2d9cc51f-6f66-40a0-973a-a9292dbee36d"
#'     download.file(file.path(url, file), destfile="fadul1-1.x3p")
#'   }
#'   x3p <- x3p_read("fadul1-1.x3p")
#'   x3p_image(x3p, size=c(500,500), zoom=.8)
#'   x3p <- x3p_circle_select(x3p, update=TRUE, col="#FF0000") 
#' 
#'   logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#'   x3p_image(logo, size=c(500,500), zoom = 1)
#'   x3p_circle_select(logo, update=TRUE, col="#00FF00") 
#' }}
x3p_circle_select <- function(x3p, col = "#FF0000", update=TRUE) {
  cat("Select 3 points on the circumference of the circle you want to mark.\n")
  stopifnot("x3p" %in% class(x3p))
  ids <- rgl::ids3d()
  if (nrow(ids) == 0) {
    size <- dim(x3p$surface.matrix)
    size[2] <- round(500/size[1]*size[2])
    size[1] <- 500
    x3p %>% x3p_image(size = size, zoom=0.8)
  }
  rgl::rgl.bringtotop()
  
 multiply <- 5
 surface <- x3p$surface.matrix
 z <- multiply * surface
 yidx <- ncol(z):1
 y <- x3p$header.info$incrementY * yidx
 x <- x3p$header.info$incrementX * (1:nrow(x3p$surface.matrix))

 xidx <- rep(x, length(y))
 yidx <- rep(y, each=length(x))


  selected <-  identify3d(xidx, yidx ,z, n=3, buttons=c("left", "middle"))
  res <- pracma::circlefit(xidx[selected], yidx[selected])
  # first two values are circle center in x and y, 
  # third value is radius
  names(res) <- c("midx", "midy", "radius")
#  browser()
  
  
  
  if (is.null(x3p$mask)) x3p <- x3p %>% x3p_add_mask()
  #  

  x3p_df <- x3p_to_df(x3p)
  incircle <- (x3p_df$x-res["midx"])^2 + (x3p_df$y-res["midy"])^2 < res["radius"]^2
#  x3p_df$mask[incircle] <- col
#  x3p <- x3p_df %>% df_to_x3p()
  x3p$mask[matrix(incircle, nrow = dim(x3p$mask)[1], byrow=TRUE)] <- col
#  browser()
  if (update) {
    rgl::rgl.bringtotop()
    clear3d() # remove the active scene
    surface3d(x, y, z, col = as.vector(x3p$mask), back = "fill")
    #  x3p %>% image_x3p(update=TRUE)
  }
  x3p
}


