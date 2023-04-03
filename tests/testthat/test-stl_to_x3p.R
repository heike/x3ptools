test_that("read STL file", {
  stl <- stl_to_x3p(system.file("pyramid.stl", package="x3ptools"))
  x3p <- x3p_read(system.file("pyramid.x3p", package="x3ptools"))
  expect_identical(stl$surface.matrix, x3p$surface.matrix)
  expect_equal(stl %>% x3p_get_scale(), x3p %>% x3p_get_scale())
})
