# x3ptools (development version)

# x3ptools 0.0.4.9000

## minor functionality

* `x3p_read_folder` allows an import of multiple x3p files (recursively) into a single data frame (in tibble format) with a list variable of x3p objects.
* `dim.x3p` just for convenience.

## bug fixes

* fix to `x3p_rotate` in an obscure situation


# x3ptools 0.0.4

## major functionality 

* `tmd_to_x3p` allows an import of a file in TMD format (natively supported by GelSight instruments) to x3p
* `x3p_bin_stripes` color-shade the mask by row or column-based gradient
* `x3p_shade_mask` color-shade the mask of an x3p by its surface profile
* `x3p_circle_select` interactively select a circle on an x3p surface by selecting three points
* update `x3p_read` and `x3p_write` to allow vendor specific information


## minor functionality 

* `x3p_extract_profile_segments` interactive function for drawing lines in segments
* added convenience parameter `create_dir` in `x3p_write` to create necessary directory structure
* `x3p_image` remove previously deprecated parameters crosscut and ccParam
* `x3p_image` change functionality of parameter size for convenience working with cartridge case scans (make window size proportional to surface), default behavior for bullet scans is unchanged. 
* updates to documentation


## bug fixes

* keep mask when averaging in `x3p_average`
* preserve annotations when casting to a data frame

# x3ptools 0.0.3

## features

* `x3p_snapshot` lets you take a snapshot of the currently active rgl device. The extension of the file determines the format.
* `x3p_mask_quantile` adds a region to the mask of an x3p object corresponding to the area between two specified quantiles
* `x3p_trim_na` trim x3p to remove missing values along the edge of the surface matrix
* `x3p_extract` subset an x3p file based on specified mask values
* `x3p_extract_profile` extract data set containing profile between two interactively identified points on the surface of the scan
* `x3p_select` interactively select a rectangle on an x3p surface 
* `x3p_fuzzyselect` repeatedly select a rectangle of interest, expand selection to include similar values based on robust linear model
* `stl_to_x3p` convert STL files of topographic surfaces to x3p files


## minor functionality 

* add functions in form of `x3p_*` to adhere to tidyverse conventions

## bug fixes

# x3ptools 0.0.2

## features

* `sample_x3p`: 
    * sample at different rates in x and y direction (using parameters `m` and `mY`)
    * sample at offset accessible through parameters `offset` and `offsetY`
* functionality to work with meta data: 
    * `x3p_show_xml` to identify and show elements of the meta file based on names
    * `x3p_modify_xml` to modify individual elements
* functionality to overlay 3d surface data with color information:    
    * adding/creating masks to overlay surfaces with color raster images    
    * `x3p_add_vline`, `x3p_add_vline`, `x3p_add_grid`: add lines on top of rendered scan surface by manipulating the mask

## minor functionality 

* testing routines added with coverage > 90%
* print.x3p and head.x3p provide a succinct summary of scan meta information
* enabled URL read of x3p files
* include svg as an option for file save from image_x3p
* read_x3p and write_x3p check xml meta file for specification of float size

## bug fixes

# x3ptools 0.0.1

initial release to CRAN
