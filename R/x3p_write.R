#' Write an x3p object to a file
#'
#' @param x3p x3p object
#' @param file path to where the file should be written
#' @param size integer. The number of bytes per element in the  surface matrix used for creating the binary file. Use size = 4 for 32 bit IEEE 754 floating point numbers and size = 8 for 64 bit IEEE 754 floating point number (default).
#' @param quiet suppress messages
#' @param create_dir boolean. create directory for saving file, if necessary. Posts a message in case a directory is created.
#' @importFrom digest digest
#' @importFrom xml2 read_xml
#' @importFrom utils as.relistable relist zip
#' @importFrom graphics par plot
#' @importFrom grDevices dev.off
#' @export
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' # write a copy of the file into a temporary file
#' x3p_write(logo, file = tempfile(fileext="x3p"))
x3p_write <- function(x3p, file, size = 8, quiet = F, create_dir = T) {
  a1 <- read_xml(system.file("templateXML.xml", package = "x3ptools"))
  a1list <- as_list(a1, ns = xml_ns(a1))
  tmp <- as.relistable(a1list) # structure needed for compiling xml_document

  # check that file can be written
  directory <- dirname(file)
  if (!dir.exists(directory)) {
    if (create_dir) {
      message("Creating directory '", directory, "'")
      dir.create(directory, recursive = TRUE)
    } else {
      stop("directory '", directory,"' does not exist. Set `create_dir` to TRUE to create it during the file export.")
    }
  }
  
  # check that size is 4 or 8
  stopifnot(size %in% c(4, 8))

  feature.info <- x3p$feature.info
  general.info <- x3p$general.info
  header.info <- x3p$header.info
  matrix.info <- x3p$matrix.info
  other.info <- x3p$other.info
  

  if (is.null(general.info)) {
    if (!quiet) message("general info not specified, using template")
    general.info <- as_list(xml_child(a1, search = "Record2"))
  }
  if (is.null(feature.info)) {
    if (!quiet) message("feature info not specified, using template")
    feature.info <- as_list(xml_child(a1, search = "Record1"))
  }
  if (is.null(matrix.info)) {
    if (!quiet) message("matrix info not specified, using template")
    matrix.info <- as_list(xml_child(a1, search = "Record3"))
  }

  # now overwrite template with relevant information:
  feature.info$Axes$CX$Increment <- list(header.info$incrementX)
  feature.info$Axes$CY$Increment <- list(header.info$incrementY)
  feature.info$Axes$CZ$Increment <- list(1)

  matrix.info$MatrixDimension$SizeX <- list(header.info$sizeX)
  matrix.info$MatrixDimension$SizeY <- list(header.info$sizeY)
  matrix.info$MatrixDimension$SizeZ <- list(1)
  # Storing the Working Dir path
  orig.path <- getwd()
  # include on.exit call to prevent surprises
  on.exit(setwd(orig.path))


  # Figure out where the file should go
  fileDir <- normalizePath(dirname(file))
  fileName <- basename(file)

  # Creating Temp directory and switch to directory
  tmpDir <- tempdir()
  tmpx3pfolder <- tempfile(pattern = "folder", tmpdir = tmpDir)
  setwd(tmpDir)

  # Set up file structure
  dir.create(tmpx3pfolder)
  setwd(tmpx3pfolder)
  dir.create("bindata")

  # Assigning values to the Record 1 part of the XML
  a1list[[1]]$Record2 <- general.info

  # Updating the Records in list for: main.xml.
  a1list[[1]]$Record3 <- matrix.info
  binPath <- file.path("bindata", "data.bin")
  a1list[[1]]$Record3$DataLink$PointDataLink <- list(binPath)
  a1list[[1]]$Record1 <- feature.info
  
  a1list[[1]]$Other <- other.info
  

  # Writing the Surface Matrix as a Binary file
  writeBin(as.vector((x3p$surface.matrix)), con = binPath, size = size)
  # Generating the MD% check sum
  chksum <- digest("bindata/data.bin", algo = "md5", serialize = FALSE, file = TRUE)
  a1list[[1]]$Record3$DataLink$MD5ChecksumPointData <- list(chksum)

  # if the mask exists, write it as a png file
  if (exists("mask", x3p)) {
    # grDevices::png(
    #   file = "bindata/mask.png", width = x3p$header.info$sizeX,
    #   height = x3p$header.info$sizeY, units = "px", #bg = "transparent" # HH: does the transparency change colors?
    # )
    # graphics::par(mar = c(0, 0, 0, 0))
    # plot(x3p$mask)
    # dev.off()
 #   browser()
    # convert_raster_to_png  is extremely slow for large scans
    # can we avoid it?
    colnames <- names(table(x3p$mask))
    if (length(colnames) > 1) {
      # does mask contain any information?
  #  browser()
      png <- convert_raster_to_png(x3p$mask) 
      
      png::writePNG(png, "bindata/mask.png")
      
    
    } else {
      # delete mask
      x3p$mask <- NULL
    }
    
    
  }

  if (size == 4) a1list[[1]]$Record1$Axes$CZ$DataType[[1]] <- "F"
  if (size == 8) a1list[[1]]$Record1$Axes$CZ$DataType[[1]] <- "D"

  # Assigning values to Record 4 in main.xml
  a1list[[1]]$Record4$ChecksumFile <- list("md5checksum.hex")

  # Convert to xml
  #  final.xml.list<- relist(unlist(a1list), skeleton = tmp) #tmp structure used for writing the xml file
  final.xml.list <- a1list
  a1xml <- as_xml_document(list(structure(list(final.xml.list))))

  # xml_attrs(a1xml)<- xml_attrs(a1)

  # Write the Main xml file
  write_xml(a1xml, "main.xml")

  # HH unfortunate solution: get namespace reference back in front of the root tag of the resulting xml:
  main <- readLines("main.xml")
  main <- gsub("^<ISO5436_2", "<p:ISO5436_2", main)
  main <- gsub("^</ISO5436_2", "</p:ISO5436_2", main)
  writeLines(main, "main.xml")

  # HH: only get the checksum once we are done changing the main.xml
  # Writing the md5checksum.hex with checksum for the main.xml
  main.chksum <- digest("main.xml", algo = "md5", serialize = FALSE, file = TRUE)
  write(main.chksum, "md5checksum.hex")

  # Write the x3p file and reset path
  # create zipped file in the specified location

  #  zip(zipfile = file.path(fileDir, fileName), files = dir())
  zip(
    zipfile = file.path(fileDir, fileName), files = dir(),
    flags = ifelse(quiet, "-r9Xq", "-r9X")
  )
  
  # not necessary to delete the temporary folder
  # setwd("..")
  #  unlink(tmpx3pfolder,recursive = TRUE)
  setwd(orig.path)
}


#' @rdname x3p_write
#' @export
write_x3p <- function(x3p, file, size = 8, quiet = F) {
  x3p_write(x3p = x3p, file = file, size = size, quiet = quiet)
}

#' Helper function
#' @param raster raster image
#' @return png object
convert_raster_to_png <- function(raster) {
  # convert a raster object to a numeric array of values between 0 and 1
  r <- raster

  mask_table <- table(r)
  splinters <- strsplit(names(mask_table), "")
  
  red <- sapply(splinters, FUN = function(s) { paste0(s[2], s[3])})
  red_values <- strtoi(red, 16L)/255
  names(red_values) <- names(table(r))
  
  green <- sapply(splinters, FUN = function(s) { paste0(s[4], s[5])})
  green_values <- strtoi(green, 16L)/255
  names(green_values) <- names(table(r))
  
  blue <- sapply(splinters, FUN = function(s) { paste0(s[6], s[7])})
  blue_values <- strtoi(blue, 16L)/255
  names(blue_values) <- names(table(r))
  
  dims <- dim(r)
  m <- array(NA,c(dims[1],dims[2],3))
  m[,,1] <- matrix(red_values[r], nrow=dims[1], ncol = dims[2], byrow = TRUE)
  m[,,2] <- matrix(green_values[r], nrow=dims[1], ncol = dims[2], byrow = TRUE)
  m[,,3] <- matrix(blue_values[r], nrow=dims[1], ncol = dims[2], byrow = TRUE)
  m  
}

