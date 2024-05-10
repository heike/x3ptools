context("x3p_crop")


test_that("x3p_crop works as expected", {
  # crop to same size
  expect_silent(tmp <- x3ptest_mask %>% x3p_crop(width=6,height=7))
  expect_equal(tmp$general.info$Comment, "cropped from location (1,1)")
  # offset element should have been added
  expect_contains(names(tmp), "offset")
  # offset should have two rows and five columns
  expect_identical(dim(tmp$offset), c(2L,5L))
  
  # try to crop to larger size
  expect_warning(tmp <- x3ptest_mask %>% x3p_crop(2, 2, width=10,height=10))
  expect_warning(tmp <- tmp %>% x3p_crop(1, 1, width=10,height=10))
  # second crop should preserve starting location within x3ptest_mask: 
  expect_equal(tmp$offset$xmin[1], 2)
  expect_equal(tmp$offset$ymin[1], 2)
  
  # crop to smaller size
  expect_silent(tmp <- x3ptest_mask %>% x3p_crop(width=4,height=4))
  
  
  expect_equivalent(
    tmp$surface.matrix,
    x3ptest_mask$surface.matrix[1:4,4:7]
  )
})

