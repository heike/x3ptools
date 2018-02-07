#' Read an x3p file into an x3p object
#' 
#' @param file The file path to the x3p file
#' @return x3p object consisting of a list of the surface matrix and the four records as specified in the ISO standard
#' @export
#' @import xml2 
#' @importFrom utils unzip
#' 
#' @examples
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
read_x3p <- function(file) {
  if (!file.exists(file)) stop(sprintf("File %f not found.\n", file))
  ## Create a temp directory to unzip x3p file
  mydir <- tempdir()
  result <- unzip(file, exdir = mydir)
  ## see what we got: 
  data <- grep(".bin$", result) # data has extension .bin
  meta <- grep(".xml$", result) # meta info has extension .xml
  # if we have not exactly one of each we have a problem:
  stopifnot(length(data)==1, length(meta) == 1) # nice error messages would be good
  
  ## Should contain data.bin and valid.bin
  bullet_data_dir <- file.path(mydir, "bindata", dir(file.path(mydir, "bindata")))
  bullet_data <- result[data]
  
  ## Get the information on the bullet
  bullet_info <- read_xml(result[meta])
  bullet_children <- xml_children(bullet_info)
  bullet_childinfo <- xml_children(bullet_children)
  
  ## Convert to a list
  bullet_info_list <- lapply(bullet_childinfo, as_list)
  bullet_info_unlist <- unlist(bullet_info_list, recursive = FALSE)
  
  ## Get the data types
  data_types <- sapply(bullet_info_list[[3]], `[[`, 2)
  
  ## Read the data matrix
  sizes <- as.numeric(c(bullet_info_unlist$SizeX[[1]], bullet_info_unlist$SizeY[[1]], bullet_info_unlist$SizeZ[[1]]))
  increments <- as.numeric(c(bullet_info_unlist$CX$Increment[[1]], bullet_info_unlist$CY$Increment[[1]], bullet_info_unlist$CZ$Increment[[1]]))
  datamat <- matrix(readBin(bullet_data, what = numeric(), n = prod(sizes[1:2])),
                    nrow = sizes[1],
                    ncol = sizes[2]) 
  
  
  ## Store some metadata
  bullet_metadata <- list(sizeY = sizes[2],
                          sizeX = sizes[1],
                          incrementY = increments[2],
                          incrementX = increments[1])
  
  input.info<- as_list(bullet_info)
  # xml2 version update
  input.info<- input.info[[1]]
  res <- list(header.info = bullet_metadata,
              surface.matrix = datamat, 
              feature.info = input.info$Record1,
              general.info= input.info$Record2,
              matrix.info = input.info$Record3)
            #  bullet_info = bullet_info)
  class(res) <- "x3p"
  return(res)
}

