context("convert")


test_that("convert works as expected", {
  x3ptest_mum <- x3p_m_to_mum(x3ptest)
  expect_equivalent(x3ptest$surface.matrix, x3ptest_mum$surface.matrix/10^6)
})
 