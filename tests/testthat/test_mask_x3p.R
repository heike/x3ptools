context("test-mask_x3p")

test_that("x3p_add_mask works", {
  expectednames <- c("surface.matrix", "header.info", "matrix.info", "mask")
  # With null mask and malformed x3p:
  x3p_mal <- x3ptest
  x3p_mal$matrix.info <- NULL
  x3ptest_mask <- x3p_add_mask(x3p_mal)
  expect_named(x3ptest_mask,
    expected = expectednames
  )
  expect_s3_class(x3ptest_mask$mask, "raster")

  # With provided mask, correct dimension
  new_mask <- x3ptest_mask$mask
  new_mask <- gsub(pattern = unique(new_mask), replacement = "#2a52be", x = new_mask)
  x3ptest_mask2 <- x3p_add_mask(x3ptest, new_mask)
  expect_named(x3ptest_mask2,
    expected = expectednames
  )
  expect_s3_class(x3ptest_mask2$mask, "raster")

  # With provided mask, wrong dimension
  new_mask2 <- x3ptest_mask$mask[-1, ]
  expect_warning(
    x3ptest_mask3 <- x3p_add_mask(x3ptest, new_mask2),
    "Mask does not have the right dimensions"
  )
  expect_named(x3ptest_mask3,
    expected = expectednames
  )
  expect_s3_class(x3ptest_mask3$mask, "raster")

  # With correct dim mask as numeric raster array (e.g. as PNG reads in)
  new_mask3 <- as.raster(array(1, dim = c(dim(new_mask), 3)))
  expect_silent(x3ptest_mask4 <- x3p_add_mask(x3ptest, new_mask3))
  expect_named(x3ptest_mask4,
    expected = expectednames
  )
  expect_s3_class(x3ptest_mask4$mask, "raster")
  # I'm not convinced it should add the mask in place - could we instead crop/pad it appropriately?

  # Test that matrix.info gets added
  x3ptest_min <- x3ptest
  x3ptest_min$matrix.info <- NULL
  x3ptest_min_mask <- x3p_add_mask(x3ptest)
  expect_failure(expect_named(x3ptest_min, expected = expectednames))
  expect_named(x3ptest_min_mask,
    expected = expectednames
  )
})

test_that("x3p_delete_mask works", {
  x3ptest_mask <- x3p_add_mask(x3ptest)

  expect_equivalent(x3p_delete_mask(x3ptest_mask), x3ptest)
})
