context("download_NIST")

setup({
  # dir.create("downloadNist")
})

teardown({
  if (dir.exists("downloadNist")) {
    unlink("downloadNist", recursive = T)
  }
})

test_that("download_NIST works as expected", {
  # Skip all tests in this block if tsapps.nist.gov/NRBTD is not reachable
  skip_if(url_unreachable("https://tsapps.nist.gov/NRBTD"))
  # skip_on_cran()
  maxfiles <- 1
  if (dir.exists("downloadNist")) {
    unlink("downloadNist", recursive = T)
  }
  expect_warning(NRBTD_download("4908a64a-702c-4203-a945-6279df3acf3f", "downloadNist", 
                               mirrorFileStructure = T, maxFiles = -1))
  expect_true(dir.exists("downloadNist"))
  expect_equal(list.files("downloadNist/", recursive = F, include.dirs = T),
               sprintf("Hi-Point %02d", 1:10))
  expect_equal(length(list.files("downloadNist", "*.x3p", recursive = T)), 0)
  unlink("downloadNist/*", recursive = T)
})

test_that("file structure mirroring = F works", {
  # Skip all tests in this block if tsapps.nist.gov/NRBTD is not reachable
  skip_if(url_unreachable("https://tsapps.nist.gov/NRBTD"))
  # skip_on_cran()
  
  maxfiles <- 1
  # File Structure Mirroring is false
  expect_silent(NRBTD_download("4908a64a-702c-4203-a945-6279df3acf3f", "downloadNist", 
                               mirrorFileStructure = F, maxFiles = maxfiles))
  expect_equal(list.files("downloadNist", recursive = F, include.dirs = T), "Hamby Hi-Point C9 Sl")
  expect_equal(length(list.files("downloadNist", "*.x3p", recursive = T)), maxfiles)
  unlink("downloadNist/*", recursive = T)
})
test_that("full download URL works", {
  # Skip all tests in this block if tsapps.nist.gov/NRBTD is not reachable
  skip_if(url_unreachable("https://tsapps.nist.gov/NRBTD"))
  # skip_on_cran()
  
  # full download link
  expect_silent(NRBTD_download("https://tsapps.nist.gov/NRBTD/Studies/Studies/Details/c09aaa86-5d60-4acb-9031-46dad2c0ad32",
                               directory = "downloadNist", mirrorFileStructure = F, maxFiles = -1))
  expect_equal(length(list.files("downloadNist", "*.x3p", recursive = T)), 0)
  unlink("downloadNist/*", recursive = T)
})
test_that("fake download URL errors", {
  # Skip all tests in this block if tsapps.nist.gov/NRBTD is not reachable
  skip_if(url_unreachable("https://tsapps.nist.gov/NRBTD"))
  # skip_on_cran()
  
  # Page that doesn't exist
  expect_error(NRBTD_download("4908a64a-702c-4203-a945-62734f3acf3f", "downloadNist", maxFiles = -1, quiet = T))
})

test_that("NRBTDsample_download works as expected", {
  # Skip all tests in this block if tsapps.nist.gov/NRBTD is not reachable
  skip_if(url_unreachable("https://tsapps.nist.gov/NRBTD"))
  # skip_on_cran()
  
  unlink("downloadNist", recursive = T)
  expect_warning(NRBTDsample_download("downloadNist", maxFiles = -1))
  expect_equal(NRBTDsample_download("downloadNist", maxFiles = 2), c(0, 0))
  expect_length(list.files("downloadNist", recursive = F, include.dirs = 2), 2)
  expect_length(list.files("downloadNist/Bullet1", "*.x3p"), 2)
  unlink("downloadNist/*", recursive = T)
})