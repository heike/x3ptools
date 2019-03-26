#' Read data from an x-y-z file
#' 
#' @param dat path to the x-y-z file
#' @param delim character determining delimiter
#' @return x3p object
#' @export
x3p_read_dat <- function (dat, delim=" ", col_names = FALSE) {
  datfile <- readr::read_delim(dat, delim=delim, col_names=col_names)
#  browser()
  
  stopifnot(dim(datfile)[2] == 3) # we need to have exactly 3 columns
  names(datfile) <- c("x", "y", "value")
  datfile$value <- as.numeric(datfile$value)
  x3p <- df_to_x3p(datfile)
  
  plux <- gsub(".dat$", ".plux", dat)
  if (file.exists(plux)) {
    x3p$general.info <- x3p_read_plux(plux)
  }
  
  x3p
}


#' Read information from plux file
#' 
#' @param plux path to plux file
#' @return xml of general information as stored in the plux file
#' @export
x3p_read_plux <- function(plux) {
#  browser()
  mydir <- tempdir()
  result <- unzip(plux, exdir = mydir)
  
  index <- read_xml(grep("index.xml", result, value=TRUE))
  
  index_info <- xml_children(xml_children(index))
  ## Convert to a list
  index_names <- sapply(index_info, xml_name)
  index_values <- sapply(index_info, xml_text)
  
  a1 <- read_xml(system.file("templateXML.xml", package="x3ptools"))
  general.info = as_list(xml_child(a1, search = "Record2"))
  
  general.info$Date[[1]] <- index_values[grep("DATE", index_names)]
  general.info$Creator[[1]] <- index_values[grep("AUTHOR", index_names)]
  
  general.info$Instrument$Manufacturer[[1]] <- "Sensofar"
  general.info$Instrument$Model[[1]] <- gsub("Device", "", grep("Device", index_values, value=TRUE))
  general.info$ProbingSystem$Type[[1]] <- gsub("Technique", "", grep("Technique", index_values, value=TRUE))
  general.info$ProbingSystem$Identification[[1]] <- gsub("Objective", "", grep("Objective", index_values, value=TRUE))
  general.info
}