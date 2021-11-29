context("read_x3p")

# Also tested in write_x3p using csafe logo
# Only need to test url portion of read_x3p here...

url <- "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/43567404-1611-4b40-ae74-a1e440e79f6a"

test_that("read_x3p works as expected", {
  skip_if_offline(host = "tsapps.nist.gov")
  skip_if(url_unreachable(url), message = "NRBTD is unreachable, skipping")
  
  tmp <- try(read_x3p(url, quiet = T), silent = T)
  skip_if(!("x3p" %in% class(tmp) ))

#  tmp <- read_x3p(url, quiet = T)
  expect_equivalent(tmp$header.info$sizeY, 1588)
  expect_equivalent(tmp$header.info$incrementY, 1.5625e-06)
  expect_equivalent(dim(tmp$surface.matrix), c(501, 1588))
  expect_equivalent(tmp$feature.info$Revision[[1]], "ISO5436 - 2000")

  # Test nonexistent file path
  expect_error(read_x3p(file.path("does", "not", "exist.x3p")))

  # Test correct reading of x3p with size=4
  expect_silent(read_x3p(tmpfile))
  expect_warning(read_x3p(tmpfile, size = 8))

  tmp <- read_x3p(tmpfile, tmpdir = "tmpx3ptest")
  expect_true(dir.exists("tmpx3ptest"))
  unlink("tmpx3ptest", recursive = T, force = T)

  # Test whether mask can be read correctly
  tmp2 <- read_x3p(tmpfile2)
})
