#' Read (or convert) from TMD file to x3p
#' 
#' TMD files are used in telemetry, specifically, they are a native format used
#' by GelSight to store 3d topographic surface scans.
#'
#' The algorithm is based on GelSight's MatLab routine `readtmd.m` published as 
#' part of the Github repository 
#' [`gelsightinc/gsmatlab`](https://github.com/gelsightinc/gsmatlab)
#' @param tmd_path path to TMD file
#' @param yaml_path path to corresponding yaml file with meta information. 
#'     If set to `NA` (default), path of the the tmd file will be tried. If set to NULL,
#'     meta file will be ignored. 
#' @param verbose boolean 
#' @return x3p file of the scan. Some rudimentary information will be filled in,  
#' information of scanning process, and parameter settings need to be added manually.
#' @importFrom assertthat assert_that
#' @importFrom yaml read_yaml
#' @export
#' @examples
#' #x3p <- tmd_to_x3p("~/Downloads/Sc04.Pl044.Ma4.SB.An80.Pb.DirFo.SizL.tmd") # 
#' #x3p <- tmd_to_x3p("~/Downloads/Sc04.Pl044.Ma4.SB.An80.Pb.DirFo.SizL.tmd", 
#' #                   yaml_path="~/Downloads/scan.yaml") # 
tmd_to_x3p <- function(tmd_path, yaml_path = NA, verbose=TRUE) {
  assert_that(file.exists(tmd_path))

  # do we care about the yaml meta information?
  if (!is.null(yaml_path)) {
    if (is.na(yaml_path)) {
      # we don't know name, so try the path of the tmd file
      directory <- dirname(tmd_path)
      yaml <- dir(directory, pattern="\\.yaml$", full.names=TRUE, recursive = FALSE)
      if (length(yaml) != 1) {
        # warning or error?
        if (length(yaml) == 0) 
          stop(simpleError(message = sprintf("No yaml file with meta information found in '%s'; set yaml_path=NULL to ignore meta information.", directory)))
        if (length(yaml) > 1)
          stop(simpleError(message = sprintf("Multiple yaml files found in directory '%s':\n %s\nAvoid ambiguity by setting yaml_path.", directory, paste(basename(yaml), sep="\n", collapse="\n "))))
      }
#      yaml_path <- yaml[1]      # can't be reached
    }
    yaml_file <- yaml::read_yaml(yaml_path)
    if (verbose) message(sprintf("Reading meta information from file '%s'.", basename(yaml_path)))
  }

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
  class(x3p) <- "x3p" # need the class early on
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
    x3p <- x3p %>% x3p_modify_xml("^Creator$", "N/A")
    if (is.null(yaml_path)) {
      # we don't want any meta info - i.e. only ensure structure is there
 #     browser()
      x3p <- x3p %>% x3p_modify_xml("^Date$", "N/A")
      x3p <- x3p %>% x3p_modify_xml("CalibrationDate", "N/A")
      x3p <- x3p %>% x3p_modify_xml("ProbingSystem.Type", "N/A")
      x3p <- x3p %>% x3p_modify_xml("Comment", sprintf("Converted from TMD file using x3ptools %s", packageVersion("x3ptools")))
    } else {
      # check that we have the correct yaml file
      if (yaml_file$activeheightmap != basename(tmd_path)) {
        warning(sprintf("Meta info in %s might not be correct for the scan. Heightmap info specifies scan '%s'. Did you rename the scan?", basename(yaml_path), yaml_file$activeheightmap))
      }
      x3p <- x3p_yaml_info(x3p, yaml_file)
    }
  }


  x3p
}

# internal helper function
# the code will likely only work for GelSight instruments.
#' @importFrom utils packageVersion
x3p_yaml_info <- function(x3p, yaml_file) {
#  browser()
  meta <- data.frame(
    Date = yaml_file$createdon,
    Instrument.Manufacturer = "GelSight",
    Instrument.Model = yaml_file$device$devicetype,
    Instrument.Serial = yaml_file$device$serialnumber,
    Instrument.Version = "v1",
    CalibrationDate = yaml_file$calibration$date,
    ProbingSystem.Type = paste0(yaml_file$metadata$appname," ", yaml_file$metadata$appversion, ", Gel: ", yaml_file$metadata$gelid, ", Use Count: ", yaml_file$metadata$gelusecount),
    ProbingSystem.Identification = yaml_file$metadata$sdkversion
  )
  i <- 1
  x3p <- x3p %>% 
    x3p_modify_xml("^Date", meta$Date[i]) %>%
    x3p_modify_xml("Instrument.Manufacturer", meta$Instrument.Manufacturer[i]) %>%
    x3p_modify_xml("Instrument.Model", meta$Instrument.Model[i]) %>% 
    x3p_modify_xml("Instrument.Serial", meta$Instrument.Serial[i]) %>%
    x3p_modify_xml("Instrument.Version", meta$Instrument.Version[i]) %>%
    x3p_modify_xml("CalibrationDate", meta$CalibrationDate[i]) %>%
    x3p_modify_xml("ProbingSystem.Type", meta$ProbingSystem.Type[i]) %>%
    x3p_modify_xml("ProbingSystem.Identification", meta$ProbingSystem.Identification[i]) 

    x3p <- x3p %>% x3p_modify_xml("Comment", sprintf("Converted from TMD file using x3ptools %s", packageVersion("x3ptools")))  
 x3p
}
