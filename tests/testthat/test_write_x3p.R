context("write_x3p")

logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))

test_that("write_x3p works as expected", {
  tmpfile <- tempfile(fileext=".x3p")
  # write a copy of the file into a temporary file
  write_x3p(logo, file = tmpfile)
  tmpx3p <- read_x3p(tmpfile)
  expect_equivalent(tmpx3p$surface.matrix, logo$surface.matrix)
  expect_equivalent(tmpx3p$header.info, logo$header.info)
  expect_equivalent(unlist(tmpx3p$feature.info)[c(1:10, 12)], unlist(logo$feature.info)[c(1:10, 12)])
  expect_equivalent(tmpx3p$feature.info$Axes$CX, logo$feature.info$Axes$CX)
  expect_equivalent(tmpx3p$feature.info$Axes$CY, logo$feature.info$Axes$CY)
  expect_equivalent(tmpx3p$feature.info$Axes$CZ$AxisType, logo$feature.info$Axes$CZ$AxisType)
  expect_equivalent(tmpx3p$feature.info$Axes$CZ$DataType, logo$feature.info$Axes$CZ$DataType)
  expect_equivalent(tmpx3p$feature.info$Axes$CZ$Offset, logo$feature.info$Axes$CZ$Offset)
  expect_equivalent(as.numeric(tmpx3p$feature.info$Axes$CZ$Increment), 
                    as.numeric(logo$feature.info$Axes$CZ$Increment)*1e6)
})
