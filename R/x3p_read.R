flatten <- function(list) {
  unlist(list, recursive = FALSE, use.names=TRUE)
}
  
#' Read an x3p file into an x3p object
#'
#' Read file in x3p format. x3p formats describe 3d topological surface according to
#' ISO standard ISO5436 – 2000.
#' x3p files are a container format implemented as a zip archive of a folder
#' consisting of an xml file of meta
#' information and a binary matrix of numeric surface measurements.
#' @param file The file path to the x3p file, or an url to an x3p file
#' @param quiet for url downloads, show download progress?
#' @param size size in bytes to use for reading the binary file. If not specified, default is used. Will be overwritten if specified in the xml meta file.
#' @param tmpdir temporary directory to use to extract the x3p file (default NULL uses tempdir() to set a directory).
#' @return x3p object consisting of a list of the surface matrix and the four records as specified in the ISO standard
#' @export
#' @import xml2
#' @importFrom utils unzip download.file
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
x3p_read <- function(file, size = NA, quiet = T, tmpdir = NULL) {
  if (grepl("http|www", file)) {
    fname <- tempfile(fileext = ".x3p")
    download.file(file, destfile = fname, quiet = quiet, mode = "wb")
    on.exit(file.remove(fname))
  } else {
    fname <- file
  }

  if (!file.exists(fname)) stop(sprintf("File %s not found.\n", fname))
  ## Create a temp directory to unzip x3p file
  if (!is.null(tmpdir)) {
    mydir <- tmpdir
  } else {
    mydir <- tempdir()
  }

  try_unzip <- try({result <- unzip(fname, exdir = mydir)}, silent=TRUE)
  if (length(result) == 0) stop(sprintf("File %s is not an x3p file", fname)) # unzipping didn't work
  ## see what we got:
  data <- grep("data.bin$", result) # data has extension .bin
  meta <- grep(".xml$", result) # meta info has extension .xml
  mask <- grep(".png$", result, value = TRUE) # mask has extension .png # for CSAFE
 # browser()
  cadre <- FALSE
  if (length(mask)==0) {
    mask <- grep("mask.bin$", result, value = TRUE) # mask has extension .png # for Cadre
    if (length(mask) > 0) cadre <- TRUE
  }
  # if we have not exactly one of each we have a problem:
  stopifnot(length(data) == 1) # nice error messages would be good

  ## Should contain data.bin and valid.bin
  bullet_data_dir <- file.path(mydir, "bindata", dir(file.path(mydir, "bindata")))
  bullet_data <- result[data]

  ## Get the meta information
  bullet_info <- lapply(result[meta], read_xml)
  bullet_children <- lapply(bullet_info, xml_children)
  bullet_childinfo <- lapply(bullet_children, xml_children)

  ## Convert to a list
  bullet_info_list <- lapply(bullet_childinfo, as_list)
  bullet_info_list <- flatten(bullet_info_list)

  bullet_info_unlist <- flatten(bullet_info_list)

  ## Read the data matrix
  sizes <- as.numeric(c(bullet_info_unlist$SizeX[[1]], bullet_info_unlist$SizeY[[1]], bullet_info_unlist$SizeZ[[1]]))
  increments <- as.numeric(
    c(
      bullet_info_unlist$CX$Increment[[1]],
      bullet_info_unlist$CY$Increment[[1]],
      ifelse(length(bullet_info_unlist$CZ$Increment) == 0, 1, bullet_info_unlist$CZ$Increment[[1]])
    )
  )
  # use a default of 1 in case the Z increment is not included

  size2 <- NA
  if (bullet_info_unlist$CZ$DataType[[1]] == "F") size2 <- 4
  if (bullet_info_unlist$CZ$DataType[[1]] == "D") size2 <- 8
  if (!is.na(size2) & !(is.na(size))) {
    if (size != size2) warning(sprintf("Number of bytes specified (%d bytes) in x3p file different from requested (%d bytes)", size2, size))
  }

  if (is.na(size)) size <- size2 # only use xml when size is not specified

  datamat <- matrix(readBin(bullet_data,
    what = numeric(),
    size = size,
    n = prod(sizes[1:2])
  ),
  nrow = sizes[1],
  ncol = sizes[2]
  )


  ## Store some metadata
  bullet_metadata <- list(
    sizeY = sizes[2],
    sizeX = sizes[1],
    incrementY = increments[2],
    incrementX = increments[1]
  )

  input.info <- flatten(lapply(bullet_info, as_list))
  if (!("Record1" %in% names(input.info))) {
    names(input.info) <- NULL
    input.info <- flatten(input.info)
  }
  # Let's make sure we have Records 1, 2, 3, and 4
  record1 <- input.info$Record1
  record2 <- input.info$Record2
  record3 <- input.info$Record3
  if (any(is.null(record1), is.null(record2), is.null(record3))) {
    warning("One of the crucial record files is missing, double check that the x3p is valid. Found Records named <", paste0(names(input.info), collapse = ","),">")
  }

  # is there missing info in general.info?
  any_empty_info <- sapply(record2, function(x) !length(x))
  if (any(any_empty_info)) {
    idx <- which(any_empty_info)
    record2[idx] <- lapply(record2[idx], function(x) {
      x <- list("")
    })
  }

  # is there any other information?
  other <- setdiff(names(input.info), c("Record1", "Record2", "Record3", "Record4"))
  record_other <- NULL
  if (length(other) > 0) {
    record_other <- input.info[other]
  }

  res <- list(
    header.info = bullet_metadata,
    surface.matrix = datamat,
    feature.info = record1,
    general.info = record2,
    matrix.info = record3,
    other.info = record_other
  )
  #  bullet_info = bullet_info)
  #  browser()

  class(res) <- "x3p"
  if (length(mask) > 0) {
    #  png <- magick::image_read(mask)
    png <- png::readPNG(mask, native = FALSE)
    if (cadre) {
      nc <- ncol(png)
      png <- png[,nc:1]
    }
    raster <- as.raster(png)
    if (!(is.na(dim(png)[3]))) {
      if (dim(png)[3] == 4) {
      # bit of a workaround - not sure why #rrggbb00 is not recognized as transparent automatically
      raster[png[, , 4] == 0] <- "transparent"
      }
    }
    #  browser()
    res <- x3p_add_mask(res, mask = raster)
  }
  return(res)
}

#' @rdname x3p_read
#' @export
read_x3p <- x3p_read
