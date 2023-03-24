#' Read (or convert) from TMD file to x3p
#' 
#' TMD files are used in telemetry, specifically, they are a native format used
#' by GelSight to store 3d topographic surface scans.
#'
#' The algorithm is based on GelSight's MatLab routine `readtmd.m` published as 
#' part of the Github repository 
#' [`gelsightinc/gsmatlab`](https://github.com/gelsightinc/gsmatlab)
#' @param path to TMD file
#' @param instrument device that created the TMD file. Defaults to GelSight.
#' @return x3p file of the scan. Some rudimentary information will be filled in,  
#' information of scanning process, and parameter settings need to be added manually.
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' x3p <- tmd_to_x3p("~/Downloads/Sc04.Pl044.Ma4.SB.An80.Pb.DirFo.SizL.tmd") 
tmd_to_x3p <- function(tmd_path, instrument=" GelSight Series 1 Mobile Probe 1.0X") {
  assert_that(file.exists(tmd_path))
  con <- file(tmd_path,"rb")
  
  header <- readBin(con, character(), n=1L, size=32)
  assert_that(header == 'Binary TrueMap Data File v2.0\r\n')

  comment <- readBin(con, character(), size=1, n=1L)

  #   % Read number of columns
  cols <- readBin(con, integer(), size=4, n=1L)  

  #   % Read number of rows
  rows <- readBin(con, integer(), size=4, n=1L)  

# #   % Length of axes # for the field of view (FOV)
#  browser()
  data.lengthx <- readBin(con, numeric(), size=4, n=1, endian="little")
  data.lengthy <- readBin(con, numeric(), size=4, n=1, endian="little")


# #   % Offsets
  data.offsetx <- readBin(con, numeric(), size=4, n=1, endian="little")
  data.offsety <- readBin(con, numeric(), size=4, n=1, endian="little")

# # surface matrix
  data.mmpp    = data.lengthx / cols # mm per pixel resolution
  hmbuf <- readBin(con, numeric(), size=4, n=rows*cols)
  close(con)

  x3p <- list()
  if (is.null(x3p$matrix.info)) {
    x3p$matrix.info <- list(MatrixDimension = list(SizeX = cols, SizeY = rows, SizeZ = 1))
  }
  x3p[["surface.matrix"]] <- t(matrix(hmbuf, rows, cols,byrow = TRUE))
  if (is.null(x3p$header.info)) {
    x3p$header.info <- list(
      sizeX = cols,
      sizeY = rows,
      incrementX = data.mmpp,
      incrementY = data.mmpp
    )
  }
  if (is.null(x3p$general.info)) {
    logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
    x3p$general.info <- logo$general.info
    x3p <- x3p %>% x3p_modify_xml("Comment", comment)
    x3p <- x3p %>% x3p_modify_xml("CalibrationDate", "N/A")
    x3p <- x3p %>% x3p_modify_xml("Creator", "Creator")
    x3p <- x3p %>% x3p_modify_xml("^Date$", "N/A")
    x3p <- x3p %>% x3p_modify_xml("Instrument", instrument)
  }
  
  class(x3p) <- "x3p"
  browser()
  
  x3p
}

