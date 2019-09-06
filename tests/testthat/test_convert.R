context("convert")


test_that("x3p_m_to_mum works as expected", {
  x3ptest_mum <- x3p_m_to_mum(x3ptest)
  expect_equivalent(x3ptest$surface.matrix, x3ptest_mum$surface.matrix / 10^6)
})

test_that("x3p_get_scale works as expected", {
  expect_silent(x3ptest_scale <- x3p_get_scale(logo))
  # TODO: Add better tests later
})
