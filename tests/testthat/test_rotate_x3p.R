context("x3p_rotate")

rotate_surface_mat_90 <- function(x) {
  t(x)[, c(rev(1:nrow(x)))]
}
### To rotate counter-clockwise
x3ptest90 <- x3p_rotate(x3ptest, angle = 90)
x3ptest_transpose <- transpose_x3p(x3ptest)
x3ptest_yflip <- y_flip_x3p(x3ptest)

test_that("x3p_rotate works as expected", {
  expect_equivalent(x3p_rotate(x3ptest, angle = 0), x3ptest)
  expect_error(
    expect_equivalent(x3ptest90$header.info, x3ptest$header.info)
  )
  expect_equivalent(x3ptest90$header.info$sizeX, x3ptest$header.info$sizeY)
  expect_equivalent(x3ptest90$header.info$sizeY, x3ptest$header.info$sizeX)
  expect_equivalent(
    x3ptest90$surface.matrix,
    rotate_surface_mat_90(x3ptest$surface.matrix)
  )
  x3ptest2 <- x3ptest %>% x3p_shade_mask()
  ### Replace NA in the mask with default color "#FFFFFF"
  x3ptest2def <- x3ptest2
  x3ptest2def$mask[is.na(x3ptest2def$mask)] <- "#FFFFFF"
  x3ptest2defb <- x3p_rotate(x3ptest2def, angle = 90)
  expect_equivalent(
    x3ptest2defb$mask,
    as.raster(t(as.matrix(x3ptest2def$mask))[rev(1:6),]) %>% toupper()
  )
})

test_that("transpose_x3p works as expected", {
  expect_equivalent(x3ptest$surface.matrix, t(x3ptest_transpose$surface.matrix))
  expect_equivalent(x3ptest$header.info$sizeX, x3ptest_transpose$header.info$sizeY)
  expect_equivalent(x3ptest$header.info$sizeY, x3ptest_transpose$header.info$sizeX)
  
  x3ptest2 <- x3ptest %>% x3p_shade_mask()
  x3ptest2b <- transpose_x3p(x3ptest2)
  expect_equivalent(
    x3ptest2b$mask,
    as.raster(t(as.matrix(x3ptest2$mask)))
  )
  
})

test_that("y_flip_x3p works as expected", {
  expect_equivalent(x3ptest_yflip$surface.matrix, x3ptest$surface.matrix[, rev(1:ncol(x3ptest$surface.matrix))])
})

test_that("x3p_flip_x works as expected", {
  x3ptest_xflip <- x3ptest %>% x3p_flip_x()
  expect_equivalent(x3ptest_xflip$surface.matrix, x3ptest$surface.matrix[rev(1:nrow(x3ptest$surface.matrix)),])
})
