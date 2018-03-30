x3ptools: working with x3p files in R
================
Heike Hofmann, Ganesh Krishnan, Eric Hare
March 30, 2018

<!--[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/x3ptools)](https://cran.r-project.org/package=x3ptools)
[![packageversion](https://img.shields.io/badge/Package%20version-0.0.1-orange.svg?style=flat-square)](commits/master)-->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Travis-CI Build Status](https://travis-ci.org/heike/x3ptools.svg?branch=master)](https://travis-ci.org/heike/x3ptools) <!--[![Last-changedate](https://img.shields.io/badge/last%20change-2018--03--30-yellowgreen.svg)](/commits/master)-->

x3ptools
========

Installation
============

`x3ptools` is available from CRAN:

``` r
install.packages("x3ptools")
```

The development version is available from Github:

``` r
# install.packages("devtools")
devtools::install_github("heike/x3ptools", build_vignettes = TRUE)
```

The file format
---------------

The x3p file format is an xml based file format created to describe digital surface measurements. x3p has been developed by OpenFMC (Open Forensic Metrology Consortium, see <http://www.openfmc.org/>) and has been adopted as ISO ISO5436 – 2000. x3p files are a zip archive of a directory consisting of an xml file of meta information and a matrix of numeric surface measurements.

### x3p objects

Internally, x3p objects are stored as a list consisting of the surface matrix (the measurements) and meta information in four records: header info, feature info, general info, and matrix info:

``` r
library(x3ptools)
logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
names(logo)
```

    ## [1] "header.info"    "surface.matrix" "feature.info"   "general.info"  
    ## [5] "matrix.info"

The four info objects specify the information for Record1 through Record4 in the xml file. An example for an xml file is provided with the package and can be accessed as `system.file("templateXML.xml", package="x3ptools")`.

`header.info` contains the information relevant to interpret locations for the surface matrix:

``` r
logo$header.info
```

    ## $sizeY
    ## [1] 419
    ## 
    ## $sizeX
    ## [1] 741
    ## 
    ## $incrementY
    ## [1] 6.45e-07
    ## 
    ## $incrementX
    ## [1] 6.45e-07

`matrix.info` expands on `header.info` and provides the link to the surface measurements in binary format.

`general.info` consists of information on how the data was captured, i.e. both author and capturing device are specified here.

`feature.info` is informed by the header info and provides the structure for storing the information.

While these pieces can be changed and adapted manually, it is more convenient to save information on the capturing device and the creator in a separate template and bind measurements and meta information together in the command `addtemplate_x3p`.

### Reading and writing x3p files

`read_x3p` and `write_x3p` are the two functions allows us to read x3p files and write to x3p files.

``` r
logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
names(logo)
```

    ## [1] "header.info"    "surface.matrix" "feature.info"   "general.info"  
    ## [5] "matrix.info"

Usage
-----

### Visualizing x3p objects

The function `image_x3p` uses `rgl` to render a 3d object in a separate window. The user can then interact with the 3d surface (zoom and rotate). In case a file name is specified in the function call the resulting surface is saved in a file (the extension determines the actual file format of the image).

``` r
image_x3p(logo, file=NULL)
```

<img src="man/figures/logo-rgl.png" width="360" />

### Casting between data types

The functions `x3p_to_df` and `df_to_x3p` allow casting between an x3p format and an x-y-z data set:

``` r
logo_df <- x3p_to_df(logo)
head(logo_df)
```

    ##           x          y value
    ## 1 0.000e+00 0.00026961 4e-07
    ## 2 6.450e-07 0.00026961 4e-07
    ## 3 1.290e-06 0.00026961 4e-07
    ## 4 1.935e-06 0.00026961 4e-07
    ## 5 2.580e-06 0.00026961 4e-07
    ## 6 3.225e-06 0.00026961 4e-07

When converting from the x3p format to a data frame, the values from the surface matrix are interpreted as heights (saved as `value`) on an x-y grid. The dimension of the matrix sets the number of different x and y levels, the information in `header.info` allows us to scale the levels to the measured quantities. Similarly, when moving from a data frame to a surface matrix, the assumption is that measurements are taken on an equi-spaced and complete grid of x-y values. The information on the resolution (i.e. the spacing between consecutive x and y locations) is saved in form of the header info, which is added to the list. General info and feature info can not be extracted from the measurements, but have to be recorded with other means.

Once data is in a regular data frame, we can use our regular means to visualize these raster images, e.g. using `ggplot2`:

``` r
library(ggplot2)
logo_df %>% ggplot(aes( x= x, y=y, fill= value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint=4e-7)
```

![](man/figures/unnamed-chunk-9-1.png)

### Elementary operations

#### Rotation and Transposition

`rotate_x3p` rotates an x3p image in steps of 90 degrees, `transpose_x3p` transposes the surface matrix of an image and updates the corresponding meta information. The function `y_flip_x3p` is a combination of transpose and rotation, that allows to flip the direction of the y axis to move easily from legacy ISO x3p scans to ones conforming to the most recent ISO standard.

#### Sampling

`sample_x3p` allows to sub-sample an x3p object to get a lower resolution image. In `sample_x3p` we need to set a sampling factor. A sample factor `m` of 2 means that we only use every 2nd value of the surface matrix, `m` of 5 means, we only use every fifth value:

``` r
dim(logo$surface.matrix)
```

    ## [1] 741 419

``` r
logo_sample <- sample_x3p(logo, m=5)
dim(logo_sample$surface.matrix)
```

    ## [1] 149  84

#### Interpolation

`interpolate_x3p` allows, like `sample_x3p`, to create a new x3p file at a new resolution, as specified in the parameters `resx` and `resy`. The new resolution should be lower (i.e. larger values for `resx` and `resy`) than the resolution specified as `IncrementX` and `IncrementY` in the header info of the x3p file. `interpolate_x3p` can also be used to interpolate missing values (set parameter `maxgap` according to specifications in `zoo::na.approx`).
