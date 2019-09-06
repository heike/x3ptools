context("x3p_add_mask_layer")

test_that("x3p_add_mask_layer works", {
  x3ptest_mask <- x3p_add_mask_layer(x3ptest, matrix(c(F, T), nrow = 7, ncol = 6), color = "blue")
  expect_s3_class(x3ptest_mask$mask, "raster")
  expect_equivalent(as.matrix(x3ptest_mask$mask), matrix(c("#000000", "blue"), nrow = 7, ncol = 6))

  expect_warning(x3p_add_mask_layer(x3ptest, matrix(c(T, F), nrow = 6, ncol = 7), color = "green"))
  expect_error(x3p_add_mask_layer("helloworld", matrix(c(T, F), nrow = 6, ncol = 7), color = "green"))
})
