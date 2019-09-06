context("sample_x3p")

x3ptest12 <- sample_x3p(x3ptest, 1, 2)
x3ptest12.1 <- sample_x3p(x3ptest, 1, 2, 0, 1)
x3ptest21 <- sample_x3p(x3ptest, 2, 1)
x3ptest21.1 <- sample_x3p(x3ptest, 2, 1, 1, 0)

x3pbig.sample <- sample_x3p(x3pbig, 1, 2)

test_that("sample_x3p works as expected", {
  expect_equivalent(dim(x3ptest12$surface.matrix), c(6, 4))
  expect_equivalent(dim(x3ptest12.1$surface.matrix), c(6, 3))
  expect_equivalent(x3ptest$surface.matrix[, 1], x3ptest12$surface.matrix[, 1])
  expect_equivalent(x3ptest$surface.matrix[, 2], x3ptest12.1$surface.matrix[, 1])
  expect_equivalent(dim(x3ptest21$surface.matrix), c(3, 7))
  expect_equivalent(dim(x3ptest21.1$surface.matrix), c(3, 7))
  expect_equivalent(x3ptest$surface.matrix[1, ], x3ptest21$surface.matrix[1, ])
  expect_equivalent(x3ptest$surface.matrix[2, ], x3ptest21.1$surface.matrix[1, ])

  expect_equivalent(dim(x3pbig.sample$surface.matrix), dim(x3pbig.sample$mask))
})
