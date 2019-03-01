#' Modify xml elements meta information in x3p object
#' 
#' Identify xml fields in the meta file of an x3p object by name and modify content if uniquely described.
#' @param x3p x3p object
#' @param element character or integer. In case of character, name of xml field in the meta file. Note that element can contain regular expressions, e.g. `"*"` returns all meta fields.
#' In case of integer, element is used as an index for the meta fields.
#' @param value character. Value to be given to the xml field in the meta file.
#' @return x3p object with changed meta information
#' @export
#' @examples 
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' x3p_show_xml(logo, "creator")
#' x3p_modify_xml(logo, "creator", "I did that") 
#' x3p_show_xml(logo, 20)
#' x3p_modify_xml(logo, 20, "I did that, too") 
x3p_modify_xml <- function(x3p, element, value) {
  find_element <- helper_identify_xml(x3p, element)
  idx <- find_element[[1]]
  res <- find_element[[2]]

  if (length(res) == 0) stop(sprintf("No element found matching <%s>", element))
  if (length(res) > 1) stop(sprintf("More than one element matching <%s> found: %s", element, paste(names(res), collapse=", ")))
  
  # we found exactly one matching meta element
  record <- NULL
  n = find_element[[3]]
  record <- which(idx <= cumsum(n))[1]

  sm <- which(names(x3p)=="surface.matrix")
  obj_str <- sprintf("x3p[-sm][[record]]$%s[[1]] <- value", gsub("\\.", "$", names(res)))
  
  eval(parse(text=obj_str))
  x3p
}


helper_identify_xml <- function(x3p, element) {
  # there are four records to look for names
  rec1 <- x3p$header.info
  rec2 <- unlist(x3p$feature.info)
  rec3 <- unlist(x3p$general.info)
  rec4 <- unlist(x3p$matrix.info)
  allrecords <- c(rec1,rec2,rec3,rec4)
  n = c(length(rec1), length(rec2), length(rec3), length(rec4))
  
  idx <- NULL
  if (is.string(element))
  idx <- grep(tolower(element), tolower(names(allrecords)))
  if (is.numeric(element)) {
    idx <- element[element >0 & element < length(allrecords)]
  }
  
  
  list(idx, allrecords[idx], n)
}

#' Show xml elements from meta information in x3p object
#' 
#' Identify xml fields by name and show content. 
#' @param x3p x3p object
#' @param element character or integer (vector). In case of character, name of xml field in the meta file. Note that element can contain regular expressions, e.g. `"*"` returns all meta fields.
#' In case of integer, element is used as an index vector for the meta fields.
#' @return list of exact field names and their contents
#' @export
#' @examples 
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' x3p_show_xml(logo, "creator") # all fields containing the word "creator"
#' x3p_show_xml(logo, "axis")
#' x3p_show_xml(logo, "CZ.AxisType")
#' # show all fields:
#' x3p_show_xml(x3p, "*")
#' # show first five fields
#' x3p_show_xml(x3p, 1:5)
x3p_show_xml <- function(x3p, element) {
  res <- helper_identify_xml(x3p, element)
  if (length(res[[2]]) == 0) warning(sprintf("no fields containing \"%s\" found. Try `element=\"*\" to see all fields.", as.character(element)))
  res[[2]]
}