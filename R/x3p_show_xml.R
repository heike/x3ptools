#' Show xml elements from meta information in x3p object
#'
#' Identify xml fields by name and show content.
#' @param x3p x3p object
#' @param element character or integer (vector). In case of character, name of xml field in the meta file. Note that element can contain regular expressions, e.g. `"*"` returns all meta fields.
#' In case of integer, element is used as an index vector for the meta fields.
#' @param verbose boolean should warning be shown?
#' @return list of exact field names and their contents
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' x3p_show_xml(logo, "creator") # all fields containing the word "creator"
#' x3p_show_xml(logo, "axis")
#' x3p_show_xml(logo, "CZ.AxisType")
#' # show all fields:
#' x3p_show_xml(logo, "*")
#' # show first five fields
#' x3p_show_xml(logo, 1:5)
x3p_show_xml <- function(x3p, element, verbose=TRUE) {
  res <- helper_identify_xml(x3p, element)
  if (length(res[[2]]) == 0) {
    if (verbose) warning(sprintf("no fields containing \"%s\" found. Try `element=\"*\" to see all fields.", as.character(element)))
    return(NA)
  }
  res[[2]]
}


#' Append a comment to the current x3p file
#'
#' Append information to the current comment of the x3p object. If there is no comment, a comment will be included in Record2 (general info)
#' @param x3p x3p object
#' @param comment character value with the appendage
#' @param sep character value to use for separating between existing and new comments
#' @return x3p object with expanded comment
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' logo %>% x3p_paste_comment("*** added to the current comment ***")
x3p_paste_comment <- function(x3p, comment, sep = ", ") {
  current <- x3p %>% x3p_show_xml(element="Comment", verbose=FALSE)
  if (is.na(current)) {
    if (is.null(x3p$general.info)) {
      x3p$general.info <- list()
    }
    if (is.null(x3p$general.info$Comment)) {
      x3p$general.info$Comment <- comment
    } 
    return(x3p)
  }
  x3p <- x3p %>% x3p_modify_xml(element="Comment", paste(current, comment, sep=sep)) 
  
  x3p
}