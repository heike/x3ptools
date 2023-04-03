library(rgl)

x3p <- read_x3p("inst/pyramid.x3p")

z <- x3p$surface.matrix


open3d()
surface3d(x=(1:5), y = (1:5), z = x3p$surface.matrix)
writeSTL("inst/pyramid.stl")


