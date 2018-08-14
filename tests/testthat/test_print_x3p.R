context("print_x3p")


test_that("print_x3p works as expected", {
  tmp <- capture_output(print(x3ptest)) %>% strsplit(split = "\\n") %>% unlist()
  expect_equal(tmp[1], "x3p object")
  expect_match(tmp[2], "size .*width x height.* \\d x \\d in pixel")
  expect_match(tmp[3], "resolution .*um x um.* \\d.* x \\d.* ")
})
