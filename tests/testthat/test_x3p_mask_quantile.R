context("x3p_mask_quantile")

test_that("x3p_mask_quantile works", {
  expect_error(x3p_mask_quantile("helloworld"))
  expect_equivalent(
    x3p_mask_quantile(x3ptest, quantiles = c(.5, 1))$mask,
    structure(c(
      "red", "red", "red", "red", "red", "red", "red",
      "red", "red", "red", "red", "red", "red", "red", "red", "#cd7f32",
      "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32",
      "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32",
      "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32", "red",
      "red", "red", "red", "red", "red", "red", "red", "red"
    ),
    .Dim = 7:6, class = "raster"
    )
  )
  expect_equivalent(
    x3p_mask_quantile(x3ptest, quantiles = .2)$mask,
    structure(c(
      "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32",
      "#cd7f32", "red", "#cd7f32", "red", "red", "red", "red", "red",
      "red", "red", "red", "red", "red", "#cd7f32", "#cd7f32", "#cd7f32",
      "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32", "#cd7f32",
      "#cd7f32", "#cd7f32", "#cd7f32", "red", "red", "red", "red",
      "red", "red", "#cd7f32", "red", "#cd7f32", "#cd7f32", "#cd7f32",
      "#cd7f32"
    ), .Dim = 7:6, class = "raster")
  )
})
