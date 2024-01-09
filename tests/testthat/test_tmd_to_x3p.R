context("tmd_to_x3p")

# Test missing values messages
test_that("tmd_to_x3p works as expected", {
  tmd_file <- system.file("testing.tmd", package="x3ptools")
  
  expect_error(tmd_to_x3p(tmd_file)) # throws error about missing yaml file
  
  expect_silent(tmd <- tmd_to_x3p(tmd_file, yaml_path = NULL))
  x3p <- x3p_read(system.file("testing.x3p", package="x3ptools"))
  expect_identical(tmd$surface.matrix, x3p$surface.matrix)

  tmd_file_2 <- system.file("gelsight/testing.tmd", package="x3ptools")
  
  expect_warning(expect_error(tmd_to_x3p(tmd_file_2, yaml_path = "doesn't exist")))
  expect_error(tmd_to_x3p(tmd_file_2)) # throws error about too many yaml files
  yaml_file <- system.file("gelsight/scan.yaml", package="x3ptools")
  expect_message(tmd2 <- tmd_to_x3p(tmd_file_2, yaml_path = yaml_file), 
    regexp="Reading meta information from file 'scan.yaml'.")
  
  yaml_file_2 <- system.file("gelsight/scan2.yaml", package="x3ptools")
  expect_warning(tmd_to_x3p(tmd_file_2, yaml_path = yaml_file_2))
  
  expect_identical(tmd$surface.matrix, tmd2$surface.matrix)
  expect_match(x3p_show_xml(tmd2, "Instrument.Model")$Instrument.Model ,"GelSight Mobile 1X")
})

