context("tmd_to_x3p")

# Test missing values messages
test_that("tmd_to_x3p works as expected", {
  tmd_file <- system.file("testing.tmd", package="x3ptools")
  
  expect_error(tmd_to_x3p(tmd_file)) # throws error about missing yaml file
  
  expect_silent(tmd <- tmd_to_x3p(tmd_file, yaml_path = NULL))
  x3p <- x3p_read(system.file("testing.x3p", package="x3ptools"))
  expect_identical(tmd$surface.matrix, x3p$surface.matrix)

})

