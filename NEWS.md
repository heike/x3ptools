# x3ptools 0.0.2

## features

- `sample_x3p`: 
    - sample at different rates in x and y direction (using parameters `m` and `mY`)
    - sample at offset accessible through parameters `offset` and `offsetY`
- functionality to work with meta data: 
    - `x3p_show_xml` to identify and show elements of the meta file based on names
    - `x3p_modify_xml` to modify individual elements
- functionality to overlay 3d surface data with color information:    
    - adding/creating masks to overlay surfaces with color raster images    
    - `x3p_add_vline`, `x3p_add_vline`, `x3p_add_grid`: add lines on top of rendered scan surface by manipulating the mask

## minor functionality 

- testing routines added with coverage > 90%
- print.x3p and head.x3p provide a succinct summary of scan meta information
- enabled URL read of x3p files
- include svg as an option for file save from image_x3p
- read_x3p and write_x3p check xml meta file for specification of float size

## bug fixes

# x3ptools 0.0.1

initial release to CRAN
