context("read_x3p")

# Also tested in write_x3p using csafe logo
# Only need to test url portion of read_x3p here...

url <- "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/43567404-1611-4b40-ae74-a1e440e79f6a"

test_that("read_x3p works as expected", {
  tmp <- read_x3p(url, quiet = T)
  expect_equivalent(tmp$header.info$sizeY, 1588)
  expect_equivalent(tmp$header.info$incrementY, 1.5625e-06)
  expect_equivalent(dim(tmp$surface.matrix), c(501, 1588))
  expect_equivalent(tmp$feature.info$Revision[[1]], "ISO5436 - 2000")
})