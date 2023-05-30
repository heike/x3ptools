context("x3p_average")


test_that("x3p_average works as expected", {
  x3ptest_avg <- x3p_average(x3ptest, b = 5)
  x3ptest_avg$surface.matrix <- round(x3ptest_avg$surface.matrix,1)
  
  expect_equivalent(
    x3ptest_avg$surface.matrix, 
    matrix(data = c(3.4, 2, 3.8, 2.9), byrow=TRUE, nrow=2)
  )
  
  test_file <- system.file("testing.x3p", package="x3ptools")
  test <- x3p_read(test_file)
  
  test_mask <- x3p_shade_mask(test, freqs = seq(0,1, by=0.2), 
                              colors = c("#134D6B", "#175d82", "#ffffff", "#d7301f", "#b12819"))
  # mask average is done as choice of most common value
  test_avg <- test_mask %>% x3p_average(b=2, f=mean, na.rm=TRUE)  
  
  mask_table <- structure(
    c(`#134D6B` = 31L, `#175d82` = 31L, `#b12819` = 32L, `#d7301f` = 28L, 
      `#ffffff` = 28L), dim = 5L, 
    dimnames = structure(
      list(c("#134D6B", "#175d82", "#b12819", "#d7301f", "#ffffff")), names = ""), class = "table")
  
  expect_identical(mask_table, table(test_avg$mask))
  
  test_mask2 <- x3p_shade_mask(test_avg, freqs = seq(0,1, by=0.2), 
                               colors = c("#134D6B", "#175d82", "#ffffff", "#d7301f", "#b12819"))
  
  # should be mostly the same between the two masks:
  expect_true(sum(test_avg$mask == test_mask2$mask, na.rm = TRUE)/prod(dim(test_mask2$surface.matrix)) > 0.9)
  
})


