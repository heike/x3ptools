context("x3p_mask_shade")

test_that("x3p_mask_shade", {
  test_file <- system.file("testing.x3p", package="x3ptools")
  test <- x3p_read(test_file)
  
  test_mask <- x3p_shade_mask(test, freqs = seq(0,1, by=0.2), 
                              colors = c("#134D6B", "#175d82", "#ffffff", "#d7301f", "#b12819"))
  expect_identical(median(table(test_mask$mask)), 120L) # 20*30/5 == 120
})

