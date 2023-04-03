#' Convert an STL file to an x3p file
#' 
#' STL (STereo Lithographic) files describe 3d objects as mesh objects. Here, we assume that the 3d object consists of a 3d surface on the top of a rectangular, equi-spaced 2d grid.
#' We further assume, that each node of the STL file describes the x-y location of an actual measurement. These measurements are then converted into the surface matrix of an x3p object.
#' The resolution is derived from the distance between consecutive x and y nodes.
#' @param stl STL file object or path to the file
#' @return x3p object
#' @importFrom rgl readSTL
#' @export
#' @examples 
#' \dontrun{
#' # the website https://touchterrain.geol.iastate.edu/ allows a download
#' # of a 3d printable terrain model. For an example we suggest to download a file from there.
#' gc <- rgl::readSTL("<PATH TO STL FILE>", plot=FALSE)
#' x3p <- stl_to_x3p(gc)
#' }
stl_to_x3p <- function(stl) {
  if (is.character(stl)) {
    stl <- rgl::readSTL(stl, plot = FALSE)
  }
  stopifnot(is.matrix(stl), ncol(stl)==3)
  
  x <- y <- value <- NULL
  
  stl_df <- data.frame(stl)
  names(stl_df) <- c("x", "y", "value")
  stl_df <- stl_df %>% group_by(x, y) %>% 
    summarize(value = max(value))
  
  
  x3p <- stl_df %>% df_to_x3p()
  x3p
}
