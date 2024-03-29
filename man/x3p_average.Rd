% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/x3p_average.R
\name{x3p_average}
\alias{x3p_average}
\title{Average an x3p object}
\usage{
x3p_average(x3p, b = 10, f = mean, ...)
}
\arguments{
\item{x3p}{x3p object}

\item{b}{positive integer value, block size}

\item{f}{function aggregate function}

\item{...}{parameters passed on to function f. Make sure to use na.rm = T as needed.}
}
\description{
Calculate blockwise summary statistics on the surface matrix of an x3p.
If the x3p object has a mask, the mode of the mask value
}
\examples{
logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
small <- x3p_average(logo)
}
