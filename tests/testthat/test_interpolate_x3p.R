context("interpolate_x3p")

tmp <- interpolate_x3p(x3ptest, resx = 1.25, resy = 1.25)

test_that("interpolate_x3p works as expected", {
  expect_warning(
    interpolate_x3p(x3ptest %>% x3p_add_mask(), resx = 1, resy = 1),
    "Mask will be deleted during interpolation. Use x3p_sample to preserve mask."
  )
  expect_warning(interpolate_x3p(x3ptest, resx = .5, resy = .5))
  expect_equivalent(dim(tmp$surface.matrix), c(5, 6))
  expect_equivalent(rowSums(is.na(tmp$surface.matrix)), c(1, 1, 1, 1, 6))
  expect_equivalent(colSums(is.na(tmp$surface.matrix)), c(1, 1, 1, 1, 1, 5))
  expect_equivalent(tmp$header.info, c(5, 6, 1.25, 1.25))
})
