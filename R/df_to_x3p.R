#' Convert an x3p file into a data frame
#' 
#' An x3p file consists of a list with meta info and a 2d matrix with scan depths. 
#' fortify turns the matrix into a data frame, using the parameters of the header as necessary.
#' @param x3p a file in x3p format as returned by function read_x3p
#' @return data frame with variables x, y, and value and meta function in attribute
#' @export
#' @examples 
#' logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
#' logo_df <- x3p_to_df(logo)
#' head(logo_df)
x3p_to_df <- function(x3p) {
  info <- x3p$header.info

  # expand.grid creates grid with first variable the fastest
  df <- data.frame(expand.grid(
    x=1:info$sizeX,
    y=info$sizeY:1
    ), 
    value=as.vector(x3p$surface.matrix))
  df$y <- (df$y-1) * info$incrementY
  df$x <- (df$x-1) * info$incrementX
  
  attr(df, "header.info") <- info
  attr(df, "feature.info") <- x3p$feature.info
  attr(df, "general.info") <- x3p$general.info
  
  df
}

#' Convert  a data frame into an x3p file
#' 
#' @param dframe  data frame. `dframe` must have the columns x, y, and value. 
#' @return x3p object
#' @importFrom stats median
#' @export
df_to_x3p <- function(dframe) {
  x3p <- attributes(dframe)[-(1:3)]
  # first three attributes are names, row.names and class, we want to keep the others

  # dframe must have columns x, y, and value  
  stopifnot(!is.null(dframe$x), !is.null(dframe$y), !is.null(dframe$value))
  
  ny <- length(unique(dframe$y))
  nx <- length(unique(dframe$x))
  if (nrow(dframe) != nx*ny) {
    cat("dframe has missing values ...")
    df2 <- expand.grid(x = unique(dframe$x), y = unique(dframe$y))
    cat(" expand ... \n")
    df2 <- merge(df2, dframe, by=c("x", "y"), all.x=TRUE)
    dframe <- df2
  }
  dframe$y <- (-1)*dframe$y
  idx <- order(dframe$x, dframe$y) 
  dframe <- dframe[idx, ]

  x3p[["surface.matrix"]] <- matrix(dframe$value, 
                                    #  nrow = ny,  ncol = nx, byrow = TRUE)
                                    nrow = nx, ncol = ny, byrow=TRUE)
  
  if (is.null(x3p$header.info)) {
    x3p$header.info <- list(sizeX = nx,
                            sizeY = ny,
                            incrementX = median(diff(unique(dframe$x))),
                            incrementY = median(diff(unique(dframe$y))))
  }
  class(x3p) <- "x3p"

  x3p
}