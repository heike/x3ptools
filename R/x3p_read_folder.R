#' Read all x3p files in a folder
#' 
#' This function is provided with a path to a folder, it will search recursively for all x3p files within the folder, read the x3p files, and return the results as part of a tibble dataset.
#' @param path character string to a folder
#' @return tibble with two variables: source contains the file path to the x3p file, and x3p is a list variable of x3p objects.
#' @export
#' @importFrom dplyr as_tibble
#' @examples
#' # example code
#' path <- dirname(system.file("csafe-logo.x3p", package="x3ptools"))
#' x3p_read_folder(path) # show all x3p files installed with x3ptools
x3p_read_folder <- function(path) {
  files <- dir(path, pattern = ".x3p$", ignore.case = TRUE, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)  
  if (length(files) == 0) {
    warning(sprintf("No x3p files found at '%s'", path))
    return(dplyr::as_tibble(data.frame("source" = NULL, "x3p" = NULL)))
  }
  read_x3p_try <- function(...) try(x3ptools::x3p_read(...))
  scans <- lapply(files, FUN = read_x3p_try)
  
  dplyr::as_tibble(data.frame(source = files, x3p = I(scans), stringsAsFactors = F))
}
