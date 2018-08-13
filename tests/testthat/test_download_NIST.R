context("download_NIST")

setup({
  dir.create("downloadNist")
})

teardown({
  unlink("downloadNist", recursive = T)
})

test_that("download_NIST works as expected", {
  # Skip all tests in this block if tsapps.nist.gov/NRBTD is not reachable
  skip_if(url_unreachable("https://tsapps.nist.gov/NRBTD"))
  maxfiles <- 2
  expect_silent(NRBTD_download("4908a64a-702c-4203-a945-6279df3acf3f", "downloadNist", 
                               mirrorFileStructure = T, maxFiles = maxfiles))
  expect_equal(list.files("downloadNist", recursive = F, include.dirs = T), 
               sprintf("Hi-Point %02d", 1:10))
  expect_equal(gsub("^(.*)\\.", "", list.files("downloadNist/Hi-Point 01/HiPoint 1-1/Hamby Hi-Point C9 Sl/", 
                                               recursive = T, include.dirs = F)),
               c("png", "png", "png", "x3p", "xlsx"))
  expect_equal(gsub("^(.*)\\.", "", list.files("downloadNist/Hi-Point 01/HiPoint 1-2/Hamby Hi-Point C9 Sl/", 
                                               recursive = T, include.dirs = F)),
               c("png", "png", "png", "x3p", "xlsx"))
  expect_equal(length(list.files("downloadNist", "*.x3p", recursive = T)), 2)
  unlink("downloadNist/*", recursive = T)
  
  # File Structure Mirroring is false
  expect_silent(NRBTD_download("4908a64a-702c-4203-a945-6279df3acf3f", "downloadNist", 
                               mirrorFileStructure = F, maxFiles = maxfiles))
  expect_equal(list.files("downloadNist", recursive = F, include.dirs = T), "Hamby Hi-Point C9 Sl")
  expect_equal(gsub("^(.*)\\.", "", list.files("downloadNist/", recursive = T, include.dirs = F)),
               c("png", "png", "png", "x3p", "png", "png", "png", "x3p", "xlsx"))
  expect_equal(length(list.files("downloadNist", "*.x3p", recursive = T)), 2)
  unlink("downloadNist/*", recursive = T)
})

test_that("NRBTDsample_download works as expected", {
  expect_equal(NRBTDsample_download("downloadNist", maxFiles = 2), c(0, 0))
  expect_length(list.files("downloadNist", recursive = F, include.dirs = 2), 2)
  expect_length(list.files("downloadNist/Bullet1", "*.x3p"), 2)
  unlink("downloadNist/*", recursive = T)
})