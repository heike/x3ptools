context("generics_x3p")

#' 
#' dim(logo)

test_that("dim.x3p works as expected", {
  logo <- x3p_read(system.file("csafe-logo.x3p", package="x3ptools"))
  expect_equal(dim(logo), c(741, 419, 1))
  
  expect_equal(dim(x3ptest_mask), c(6, 7, 2))
  
})

