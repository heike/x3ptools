#' Write an x3p object to a file
#' 
#' @param x3p x3p object
#' @param file path to where the file should be written
#' @importFrom digest digest
#' @importFrom xml2 read_xml
#' @importFrom utils as.relistable relist zip
#' @export
#' @examples
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' # write a copy of the file into a temporary file
#' write_x3p(logo, file = tempfile(fileext="x3p")) 
write_x3p <- function(x3p, file)
{
  a1 <- read_xml(system.file("templateXML.xml", package="x3ptools"))
  a1list<- as_list(a1, ns = xml_ns(a1))
  tmp<- as.relistable(a1list) # structure needed for compiling xml_document
  
  feature.info <- x3p$feature.info
  general.info <- x3p$general.info
  header.info <- x3p$header.info
  matrix.info <- x3p$matrix.info
  
  if (is.null(general.info)) {
    cat("general info not specified, using template\n")
    general.info = as_list(xml_child(a1, search = "Record2"))
  }
  if (is.null(feature.info)) {
    cat("feature info not specified, using template\n")
    feature.info = as_list(xml_child(a1, search = "Record1"))
    
  }
  if (is.null(matrix.info)) {
    cat("matrix info not specified, using template\n")
    matrix.info = as_list(xml_child(a1, search = "Record3"))
    
  }
  
  # now overwrite template with relevant information:
  feature.info$Axes$CX$Increment <- list(header.info$incrementX)
  feature.info$Axes$CY$Increment <- list(header.info$incrementY)
  feature.info$Axes$CZ$Increment <- list(1)
  
  matrix.info$MatrixDimension$SizeX <- list(header.info$sizeX)
  matrix.info$MatrixDimension$SizeY <- list(header.info$sizeY)
  matrix.info$MatrixDimension$SizeZ <- list(1)
  # Storing the Working Dir path
  orig.path<- getwd()
  # include on.exit call to prevent surprises
  on.exit(setwd(orig.path))
  

  # Figure out where the file should go
  fileDir <- normalizePath(dirname(file))
  fileName <- basename(file)

  # Creating Temp directory and switch to directory
  tmpDir <- tempdir()
  tmpx3pfolder <- tempfile(pattern="folder", tmpdir=tmpDir)
  setwd(tmpDir)
  
  # Set up file structure
  dir.create(tmpx3pfolder)
  setwd(tmpx3pfolder)
  dir.create("bindata")
  
  # Assigning values to the Record 1 part of the XML
  a1list[[1]]$Record2 <- general.info
  
  # Updating the Records in list for: main.xml.
  a1list[[1]]$Record3 <- matrix.info
  binPath = file.path("bindata", "data.bin")
  a1list[[1]]$Record3$DataLink$PointDataLink <- list(binPath)
  a1list[[1]]$Record1 <- feature.info
  
  # Writing the Surface Matrix as a Binary file
  writeBin(as.vector((x3p$surface.matrix)), con = binPath)
  
  # Generating the MD% check sum
  chksum<- digest("bindata/data.bin", algo= "md5", serialize=FALSE, file=TRUE)
  a1list[[1]]$Record3$DataLink$MD5ChecksumPointData <- list(chksum)
  
  
  # Assigning values to Record 4 in main.xml
  a1list[[1]]$Record4$ChecksumFile <- list("md5checksum.hex")
  
  # Convert to xml
  #  final.xml.list<- relist(unlist(a1list), skeleton = tmp) #tmp structure used for writing the xml file
  final.xml.list <- a1list
  a1xml<- as_xml_document(list(structure(list(final.xml.list))))
  
  #xml_attrs(a1xml)<- xml_attrs(a1)
  
  # Write the Main xml file
  write_xml(a1xml, "main.xml")
  
  # HH unfortunate solution: get namespace reference back in front of the root tag of the resulting xml:
  main <- readLines("main.xml")
  main <- gsub("^<ISO5436_2", "<p:ISO5436_2", main)
  main <- gsub("^</ISO5436_2", "</p:ISO5436_2", main)
  writeLines(main, "main.xml")
  
  # HH: only get the checksum once we are done changing the main.xml
  # Writing the md5checksum.hex with checksum for the main.xml
  main.chksum<- digest("main.xml", algo= "md5", serialize=FALSE, file=TRUE)
  write(main.chksum, "md5checksum.hex")
  
  # Write the x3p file and reset path
  # create zipped file in the specified location 
  
  zip(zipfile = file.path(fileDir, fileName), files = dir())
  # not necessary to delete the temporary folder 
 # setwd("..")
#  unlink(tmpx3pfolder,recursive = TRUE)
  setwd(orig.path) 
  
}


