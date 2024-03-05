context("x3p_read_folder")


test_that("x3p_read_folder works as expected", {

  # Test nonexistent file path
  expect_warning(x3p_read_folder(file.path("does", "not", "exist")))

  # Test files installed with x3ptools
  path <- dirname(system.file("csafe-logo.x3p", package="x3ptools"))
  res <- x3p_read_folder(path) # show all x3p files installed with x3ptools
  expect_equal(names(res), c("source", "x3p"))
  expect_equal(class(res$x3p[[1]]), "x3p")
})
