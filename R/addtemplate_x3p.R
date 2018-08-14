#' Add/change xml meta information in x3p object
#' 
#' Use the specified template to overwrite the general info in the x3p object (and structure of the feature info, if needed).
#' @param x3p x3p object
#' @param template file path to xml template, use NULL for in-built package template
#' @export
#' @examples 
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' # exchange meta information for general x3p information:
#' logo <- addtemplate_x3p(logo, template = system.file("templateXML.xml", package="x3ptools"))
#' logo$general.info
addtemplate_x3p <- function(x3p, template = NULL) {
  if (is.null(template)) {
    template <- system.file("templateXML.xml", package = "x3ptools")
  }
  a1 <- read_xml(template)
  a1list <- as_list(a1)
  
  x3p$general.info <- a1list[[1]]$Record2
  if (is.null(x3p$feature.info)) {
    x3p$feature.info <- a1list[[1]]$Record1
    x3p$feature.info$Axes$CX$Increment <- x3p$header.info$incrementX
    x3p$feature.info$Axes$CY$Increment <- x3p$header.info$incrementY
  }
  x3p
}