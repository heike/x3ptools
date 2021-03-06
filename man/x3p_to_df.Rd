% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_to_x3p.R
\name{x3p_to_df}
\alias{x3p_to_df}
\title{Convert an x3p file into a data frame}
\usage{
x3p_to_df(x3p)
}
\arguments{
\item{x3p}{a file in x3p format as returned by function x3p_read}
}
\value{
data frame with variables x, y, and value and meta function in attribute
}
\description{
An x3p file consists of a list with meta info and a 2d matrix with scan depths.
fortify turns the matrix into a data frame, using the parameters of the header as necessary.
}
\examples{
logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
logo_df <- x3p_to_df(logo)
head(logo_df)
}
