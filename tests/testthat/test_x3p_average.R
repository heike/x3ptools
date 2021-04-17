context("x3p_average")


test_that("x3p_average works as expected", {
  x3ptest_avg <- x3p_average(x3ptest, b = 5)
  x3ptest_avg$surface.matrix <- round(x3ptest_avg$surface.matrix,1)
  
  expect_equivalent(
    x3ptest_avg$surface.matrix, 
    matrix(data = c(3.4, 2, 3.8, 2.9), byrow=TRUE, nrow=2)
  )
  
})


