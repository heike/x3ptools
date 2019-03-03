# x3ptools 0.0.2

## features

- sample_x3p: 
    - sample at different rates in x and y direction (using parameters m and mY)
    - sample at offset accessible through parameters offset and offsetY
- print.x3p for showing a concise summary of relevant meta information from the xml file. 
- download sample x3p files from the NIST Ballistics Toolmarks Research Database
- x3p_show_xml and x3p_modify_xml:
    - identify and show elements of the meta file based on names
    - modify individual elements
- image_x3p_grid add gridlines on top of rendered scan images.    

## minor functionality 

- testing routines added with coverage > 90%
- head.x3p provides a succinct summary of scan meta information
- enabled URL read of x3p files
- include svg as an option for file save from image_x3p
- read_x3p and write_x3p check xml meta file for specification of float size

## bug fixes

# x3ptools 0.0.1

initial release to CRAN
