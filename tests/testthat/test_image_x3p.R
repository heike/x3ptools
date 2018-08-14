context("image_x3p")

teardown({
  if (file.exists("x3ptest.png")) {
    file.remove("x3ptest.png")
  }
  if (file.exists("x3ptest.stl")) {
    file.remove("x3ptest.stl")
  }
})

test_that("image_x3p works as expected", {
  expect_error(image_x3p("hello world"), ".x3p. .in. class.*x3p.* is not TRUE")
  image_x3p(x3ptest)
  rglwindowopen <- rgl::.check3d()
  # Check that a window is open
  expect_equivalent(rglwindowopen, 1)
  # If open, close it
  if (rglwindowopen) {
    rgl::rgl.close()
  }
  image_x3p(x3ptest, file = "x3ptest.png")
  expect_true(file.exists("x3ptest.png"))
  
  image_x3p(x3ptest, file = "x3ptest.stl")
  expect_true(file.exists("x3ptest.stl"))
})
