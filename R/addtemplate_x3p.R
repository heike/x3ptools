#' Add/change xml meta information in x3p object
#'
#' Use a specified template to overwrite the general info in the x3p object (and structure of the feature info, if needed).
#' @param x3p x3p object
#' @param template file path to xml template, use NULL for in-built package template
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' # exchange meta information for general x3p information:
#' logo <- x3p_add_meta(logo, template = system.file("templateXML.xml", package="x3ptools"))
#' logo$general.info
x3p_add_meta <- function(x3p, template = NULL) {
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

#' @rdname x3p_add_meta
#' @export
addtemplate_x3p <- function(x3p, template = NULL) {
  x3p_add_meta(x3p = x3p, template = template)
}
