#' Convert an x3p file into a data frame
#'
#' An x3p file consists of a list with meta info and a 2d matrix with scan depths.
#' fortify turns the matrix into a data frame, using the parameters of the header as necessary.
#' @param x3p a file in x3p format as returned by function x3p_read
#' @return data frame with variables x, y, and value and meta function in attribute
#' @export
#' @importFrom dplyr select
#' @examples
#' logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
#' logo_df <- x3p_to_df(logo)
#' head(logo_df)
x3p_to_df <- function(x3p) {
  info <- x3p$header.info
  if (is.null(info$sizeX)) {
    if (!is.null(info$num_obs_per_profile)) {
      info$sizeX <- info$num_obs_per_profile
    } else {
      warning("Assuming X is represented by rows of the surface matrix because it is not specified in header information")
      info$sizeX <- nrow(x3p$surface.matrix)
    }
  }
  if (is.null(info$sizeY)) {
    if (!is.null(info$num_profiles)) {
      info$sizeY <- info$num_profiles
    } else {
      warning("Assuming Y is represented by columns of the surface matrix because it is not specified in header information")
      info$sizeY <- ncol(x3p$surface.matrix)
    }
  }
  if (is.null(info$incrementY)) {
    if (!is.null(info$profile_inc)) {
      info$incrementY <- info$profile_inc
    } else {
      warning("Assuming Y increment is 1 - not specified")
      info$incrementY <- 1
    }
  }
  if (is.null(info$incrementX)) {
    if (!is.null(info$obs_inc)) {
      info$incrementX <- info$obs_inc
    } else {
      warning("Assuming X increment is 1 - not specified")
      info$incrementX <- 1
    }
  }

  # expand.grid creates grid with first variable the fastest
  df <- data.frame(expand.grid(
    x = 1:info$sizeX,
    y = info$sizeY:1
  ),
  value = as.vector(x3p$surface.matrix)
  )
  df$y <- (df$y - 1) * info$incrementY
  df$x <- (df$x - 1) * info$incrementX

  if (!is.null(x3p$mask)) {
    df$mask <- as.vector(x3p$mask)
    # make sure the hex code is lower case and only 6 digits wide (7 including the hash)
    df$maskmerge <- tolower(substr(df$mask, 1, 7))
    annotations <- x3p_mask_legend(x3p)
    if (!is.null(annotations)) {
      legend <- data.frame(maskmerge = annotations, annotation = names(annotations))
      legend$maskmerge <- tolower(legend$maskmerge)
      df <- merge(df, legend, by = "maskmerge", all.x = TRUE)
      df <- select(df, -"maskmerge")
    }
  }

  attr(df, "header.info") <- info
  attr(df, "feature.info") <- x3p$feature.info
  attr(df, "general.info") <- x3p$general.info

  df
}

#' Convert  a data frame into an x3p file
#'
#' @param dframe  data frame. `dframe` must have the columns x, y, and value.
#' @return x3p object
#' @param var name of the variable containing the surface measurements. Defaults to "value".
#' @importFrom stats median
#' @importFrom dplyr pull
#' @export
df_to_x3p <- function(dframe, var = "value") {
  x3p <- attributes(dframe)[-(1:3)]
  # first three attributes are names, row.names and class, we want to keep the others
  if (var != "value") dframe$value <- pull(dframe, var)
  
  # dframe must have columns x, y, and value
  stopifnot(!is.null(dframe$x), !is.null(dframe$y), !is.null(dframe$value))

  ny <- length(unique(dframe$y))
  nx <- length(unique(dframe$x))
  if (nrow(dframe) != nx * ny) {
    message("dframe has missing values ... they will be expanded")
    df2 <- expand.grid(x = unique(dframe$x), y = unique(dframe$y))
    df2 <- merge(df2, dframe, by = c("x", "y"), all.x = TRUE)
    dframe <- df2
  }
  dframe$y <- (-1) * dframe$y
  idx <- order(dframe$x, dframe$y)
  dframe <- dframe[idx, ]

  x3p[["surface.matrix"]] <- matrix(dframe$value,
    #  nrow = ny,  ncol = nx, byrow = TRUE)
    nrow = nx, ncol = ny, byrow = TRUE
  )

  if (is.null(x3p$header.info)) {
    x3p$header.info <- list(
      sizeX = nx,
      sizeY = ny,
      incrementX = median(diff(unique(dframe$x))),
      incrementY = median(diff(unique(dframe$y)))
    )
  }

  if (is.null(x3p$matrix.info)) {
    x3p$matrix.info <- list(MatrixDimension = list(SizeX = nx, SizeY = ny, SizeZ = 1))
  }
  class(x3p) <- "x3p"
  
  if ("mask" %in% names(dframe)) {
    x3p <- x3p %>% x3p_add_mask(mask = matrix(dframe$mask, nrow = dim(x3p$surface.matrix)[2]))
  }

  x3p
}


#' Convert an STL file to an x3p file
#' 
#' STL files describe 3d objects as mesh objects. Here, we assume that the 3d object consists of a 3d surface on the top of a rectangular, equi-spaced 2d grid.
#' We further assume, that each node of the STL file describes the x-y location of an actual measurement. These measurements are then converted into the surface matrix of an x3p object.
#' The resolution is derived from the distance between consecutive x and y nodes.
#' @param stl STL file object or path to the file
#' @return x3p object
#' @importFrom rgl readSTL
#' @export
#' @examples 
#' \dontrun{
#' # the website https://touchterrain.geol.iastate.edu/ allows a download
#' # of a 3d printable terrain model. For an example we suggest to download a file from there.
#' gc <- rgl::readSTL("<PATH TO STL FILE>", plot=FALSE)
#' x3p <- stl_to_x3p(gc)
#' }
stl_to_x3p <- function(stl) {
  if (is.character(stl)) {
    stl <- rgl::readSTL(stl, plot = FALSE)
  }
  stopifnot(is.matrix(stl), ncol(stl)==3)

  x <- y <- value <- NULL
  
  stl_df <- data.frame(stl)
  names(stl_df) <- c("x", "y", "value")
  stl_df <- stl_df %>% group_by(x, y) %>% 
    summarize(value = max(value))


  x3p <- stl_df %>% df_to_x3p()
  x3p
}
