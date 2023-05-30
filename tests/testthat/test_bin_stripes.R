context("x3p_bin_stripes")

test_that("x3p_bin_stripes", {
  test_file <- system.file("testing.x3p", package="x3ptools")
  test <- x3p_read(test_file)
  
  mask_horizontal_table <- structure(
    c(`#134D6B` = 28L, `#175d82` = 28L, `#5186a2` = 114L, 
      `#b12819` = 29L, `#d7301f` = 28L, `#e16457` = 114L, `#ffffff` = 228L
  ), dim = 7L, 
  dimnames = list(
    mask_horizontal = c("#134D6B", "#175d82", "#5186a2", "#b12819", 
                        "#d7301f", "#e16457", "#ffffff")), class = "table")
                        
  mask_horizontal <- x3p_bin_stripes(test, direction = "horizontal")$mask  
  
  expect_identical(table(mask_horizontal),
                   mask_horizontal_table)
  
  mask_file <- system.file("mask-vertical.rds", package="x3ptools")
  mask_vertical <- readRDS(mask_file)
  expect_identical(
    x3p_bin_stripes(test, direction = "vertical")$mask,
    mask_vertical)                                                          
})

