context("write_x3p")


test_that("write_x3p works as expected", {
  tmpfile <- tempfile(fileext = ".x3p")
  # write a copy of the file into a temporary file
  write_x3p(x3ptest, file = tmpfile)
  tmpx3p <- read_x3p(tmpfile)
  expect_equivalent(tmpx3p$surface.matrix, x3ptest$surface.matrix)
  expect_equivalent(tmpx3p$header.info, x3ptest$header.info)
  
  file.remove(tmpfile)
})