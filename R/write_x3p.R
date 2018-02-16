#' Write an x3p object to a file
#' 
#' @param x3p x3p object
#' @param file path to where the file should be written
#' @importFrom digest digest
#' @importFrom xml2 read_xml
#' @importFrom utils as.relistable relist zip
#' @export
write_x3p <- function(x3p, file)
{
  a1 <- read_xml(system.file("templateXML.xml", package="x3ptools"))
  a1list<- as_list(a1)
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
  
  feature.info$Axes$CX$Increment <- header.info$incrementX
  feature.info$Axes$CY$Increment <- header.info$incrementY
  feature.info$Axes$CZ$Increment <- 1 
  
  matrix.info$MatrixDimension$SizeX <- header.info$sizeX
  matrix.info$MatrixDimension$SizeY <- header.info$sizeY
  matrix.info$MatrixDimension$SizeZ <- 1
  # Storing the Working Dir path
  orig.path<- getwd()
  # Creating Temp directory and bin directory
  # 'File structure'
  dir.create("x3pfolder")
  dir.create("x3pfolder/bindata")
  
  # Change Working Dir 
  setwd(paste0(getwd(),"/x3pfolder"))
  new.wdpath<- getwd()
  # Assigning values to the Record 1 part of the XML
  a1list[[1]]$Record2 <- general.info
  
  sizes<- c(matrix.info$MatrixDimension$SizeX, matrix.info$MatrixDimension$SizeY, matrix.info$MatrixDimension$SizeZ)
  sizes<- as.numeric(sizes)
  increments<- c(feature.info$Axes$CX$Increment, feature.info$Axes$CY$Increment, feature.info$Axes$CZ$Increment)
  increments<- as.numeric(increments)
  
  # Updating the list values
  matrix.info$MatrixDimension$SizeX<- as.character(sizes[1])
  matrix.info$MatrixDimension$SizeY<- as.character(sizes[2])
  matrix.info$MatrixDimension$SizeZ<- as.character(sizes[3])
  feature.info$Axes$CX$Increment<- as.character(increments[1])
  feature.info$Axes$CY$Increment<- as.character(increments[2])
  feature.info$Axes$CZ$Increment<- as.character(increments[3])
  
  # Updating the Records in list for: main.xml.
  a1list[[1]]$Record3 <- matrix.info
  a1list[[1]]$Record3$DataLink$PointDataLink<- "bindata/data.bin"
  a1list[[1]]$Record1 <- feature.info
  
  
  
  # Writing the Surface Matrix as a Binary file
  writeBin(as.vector((x3p$surface.matrix)), con = "bindata/data.bin")
  
  # Generating the MD% check sum
  chksum<- digest("bindata/data.bin", algo= "md5", serialize=FALSE, file=TRUE)
  a1list[[1]]$Record3$DataLink$MD5ChecksumPointData<- chksum
  
  
  # Assigning values to Record 4 in main.xml
  a1list[[1]]$Record4$ChecksumFile<- "md5checksum.hex"
  
  # Convert to xml
  final.xml.list<- relist(unlist(a1list), skeleton = tmp) #tmp structure used for writing the xml file
  a1xml<- as_xml_document(list(structure(list(final.xml.list))))
  
  #xml_attrs(a1xml)<- xml_attrs(a1)
  
  # Write the Main xml file
  write_xml(a1xml, "main.xml")
  
  # Writing the md5checksum.hex with checksum for the main.xml
  main.chksum<- digest("main.xml", algo= "md5", serialize=FALSE, file=TRUE)
  write(main.chksum, "md5checksum.hex")
  
  # Write the x3p file and reset path
  # create zipped file one level up, now get out and delete
  zip(zipfile = paste0("../",file), files = dir())
  setwd("./..")
  unlink("x3pfolder",recursive = TRUE)
  
  setwd(orig.path) 
  
}