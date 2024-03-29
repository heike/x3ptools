% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmd_to_x3p.R
\name{tmd_to_x3p}
\alias{tmd_to_x3p}
\title{Read (or convert) from TMD file to x3p}
\usage{
tmd_to_x3p(tmd_path, yaml_path = NA, verbose = TRUE)
}
\arguments{
\item{tmd_path}{path to TMD file}

\item{yaml_path}{path to corresponding yaml file with meta information.
If set to \code{NA} (default), path of the the tmd file will be tried. If set to NULL,
meta file will be ignored.}

\item{verbose}{boolean}
}
\value{
x3p file of the scan. Some rudimentary information will be filled in,
information of scanning process, and parameter settings need to be added manually.
}
\description{
TMD files are used in telemetry, specifically, they are a native format used
by GelSight to store 3d topographic surface scans.
}
\details{
The algorithm is based on GelSight's MatLab routine \code{readtmd.m} published as
part of the Github repository
\href{https://github.com/gelsightinc/gsmatlab}{\code{gelsightinc/gsmatlab}}
}
\examples{
#x3p <- tmd_to_x3p("~/Downloads/Sc04.Pl044.Ma4.SB.An80.Pb.DirFo.SizL.tmd") # 
#x3p <- tmd_to_x3p("~/Downloads/Sc04.Pl044.Ma4.SB.An80.Pb.DirFo.SizL.tmd", 
#                   yaml_path="~/Downloads/scan.yaml") # 
}
