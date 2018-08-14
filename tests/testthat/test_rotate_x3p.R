context("rotate_x3p")

rotate_surface_mat_90 <- function(x) {
  t(x)[,c(rev(1:nrow(x)))]
}
x3ptest90 <- rotate_x3p(x3ptest)

test_that("rotate_x3p works as expected", {
  expect_equivalent(rotate_x3p(x3ptest, angle = 0), x3ptest)
  expect_error(
    expect_equivalent(x3ptest90$header.info, x3ptest$header.info)
  )
  expect_equivalent(x3ptest90$header.info$sizeX, x3ptest$header.info$sizeY)
  expect_equivalent(x3ptest90$header.info$sizeY, x3ptest$header.info$sizeX)
  expect_equivalent(x3ptest90$surface.matrix, 
                    rotate_surface_mat_90(x3ptest$surface.matrix))
})
