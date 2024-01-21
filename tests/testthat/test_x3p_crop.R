context("x3p_crop")


test_that("x3p_crop works as expected", {
  # crop to same size
  expect_silent(tmp <- x3ptest_mask %>% x3p_crop(width=6,height=7))
  # try to crop to larger size
  expect_warning(tmp <- x3ptest_mask %>% x3p_crop(width=10,height=10))
  # crop to smaller size
  expect_silent(tmp <- x3ptest_mask %>% x3p_crop(width=4,height=4))
  
  expect_equivalent(
    tmp$surface.matrix,
    x3ptest_mask$surface.matrix[1:4,4:7]
  )
})

