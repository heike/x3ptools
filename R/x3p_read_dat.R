#' Read data from an x-y-z file
#'
#' @param dat path to the x-y-z file
#' @param delim character determining delimiter
#' @param col_names logical value - does the first line of the file contain the column names? Default is set to FALSE.
#' @return x3p object
#' @importFrom readr read_delim
#' @export
x3p_read_dat <- function(dat, delim = " ", col_names = FALSE) {
  stopifnot(file.exists(dat))
  datfile <- readr::read_delim(dat, delim = delim, col_names = col_names)
  #  browser()

  stopifnot(dim(datfile)[2] == 3) # we need to have exactly 3 columns
  names(datfile) <- c("x", "y", "value")
  datfile$value <- as.numeric(datfile$value)
  x3p <- df_to_x3p(datfile)

  plux <- gsub(".dat$", ".plux", dat)
  if (file.exists(plux)) {
    x3p$general.info <- x3p_read_plux(plux)
  } else {
    warning(sprintf("no matching plux file found for %s", dat))
  }

  x3p
}


#' Read information from plux file
#'
#' plux files are zip containers of 3d topographic scans in a format proprietary to Sensofar™. 
#' One of the files in the container is the file `index.xml` which contains meta-information on the instrument, scan settings, date, and creator.   
#' This information is added to the x3p meta-information.
#' @param plux path to plux file
#' @return xml of general information as stored in the plux file
#' @export
x3p_read_plux <- function(plux) {
  #  browser()
  mydir <- tempdir()
  result <- unzip(plux, exdir = mydir)

  index <- read_xml(grep("index.xml", result, value = TRUE))

  index_info <- xml_children(xml_children(index))
  ## Convert to a list
  index_names <- sapply(index_info, xml_name)
  index_values <- sapply(index_info, xml_text)

  a1 <- read_xml(system.file("templateXML.xml", package = "x3ptools"))
  general.info <- as_list(xml_child(a1, search = "Record2"))

  general.info$Date[[1]] <- index_values[grep("DATE", index_names)]
  general.info$Creator[[1]] <- index_values[grep("AUTHOR", index_names)]

  general.info$Instrument$Manufacturer[[1]] <- "Sensofar"
  general.info$Instrument$Model[[1]] <- gsub("Device", "", grep("Device", index_values, value = TRUE))
  general.info$ProbingSystem$Type[[1]] <- gsub("Technique", "", grep("Technique", index_values, value = TRUE))
  general.info$ProbingSystem$Identification[[1]] <- gsub("Objective", "", grep("Objective", index_values, value = TRUE))
  general.info
}
