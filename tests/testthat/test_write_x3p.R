context("write_x3p")


test_that("write_x3p works as expected", {
  tmpfile <- tempfile(fileext = ".x3p")
  # write a copy of the file into a temporary file
  write_x3p(x3ptest, file = tmpfile, quiet = T)
  # make it chatty
  x3ptest2 <- x3ptest
  x3ptest2$matrix.info <- NULL
  expect_message(
    write_x3p(x3ptest, file = tmpfile, quiet = F), 
    regexp="general info not specified, using template")
  
  tmpx3p <- read_x3p(tmpfile)
  expect_equivalent(tmpx3p$surface.matrix, x3ptest$surface.matrix)
  expect_equivalent(tmpx3p$header.info$sizeX, x3ptest$header.info$sizeX)
  expect_equivalent(tmpx3p$header.info$sizeY, x3ptest$header.info$sizeY)
  expect_equivalent(tmpx3p$header.info$incrementX, x3ptest$header.info$incrementX)
  expect_equivalent(tmpx3p$header.info$incrementY, x3ptest$header.info$incrementY)

  file.remove(tmpfile)
})
