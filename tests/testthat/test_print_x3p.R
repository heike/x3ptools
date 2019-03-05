context("print_x3p")


scientific <- "(?:0\\.(?:0[1-9]|[1-9]\\d?)|[1-9]\\d*(?:\\.\\d{1,2})?)(?:e[+-]?\\d+)?"

test_that("print_x3p works as expected", {
  tmp <- capture_output(print(x3ptest)) %>% strsplit(split = "\\n") %>% unlist()
  expect_equal(tmp[1], "x3p object")
  expect_match(tmp[2], "size .width x height.: \\d{1,} x \\d{1,} in pixel ")
  expect_match(tmp[3], sprintf("resolution.*%s.*x.*%s.*", scientific, scientific))
})
