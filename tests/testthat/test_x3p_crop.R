context("x3p_crop")


test_that("x3p_crop works as expected", {
  expect_silent(tmp <- x3ptest_mask %>% x3p_crop(width=4,height=4))
  
  expect_equivalent(
    tmp$surface.matrix,
    x3ptest_mask$surface.matrix[1:4,3:6]
  )
})

