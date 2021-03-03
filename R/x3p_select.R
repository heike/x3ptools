#' Draw rectangle on the surface of an x3p file using rgl
#' 
#' 
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
#' }
x3p_select <- function(x3p, col = "#FF0000", update=TRUE) {
  #  browser()
  stopifnot("x3p" %in% class(x3p))
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
    pop3d() # remove the active scene
    surface3d(x, y, z, as.vector(x3p$mask), back = "fill")
  }
  x3p
}