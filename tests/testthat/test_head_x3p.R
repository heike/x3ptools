context("head_x3p")

setup({
  
})
teardown({
  file.remove("head_test_output.txt")
})

head_output <- capture_output(head_obj <- head(x3ptest))
test_that("head_x3p works as expected", {
  expect_equivalent(head(x3ptest$surface.matrix)[,1:6], head_obj)
  expect_match(head_output, "size .*width x height.* \\d{1,} x \\d{1,} in pixel")
  expect_match(head_output, ".*resolution .*um x um.* \\d{1,}.* x \\d{1,}.* ")
})
