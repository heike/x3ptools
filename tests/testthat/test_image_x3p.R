context("image_x3p")

teardown({
  if (file.exists("x3ptest.png")) {
    file.remove("x3ptest.png")
  }
  if (file.exists("x3ptest.svg")) {
    file.remove("x3ptest.svg")
  }
  if (file.exists("x3ptest.stl")) {
    file.remove("x3ptest.stl")
  }
  if (file.exists("x3ptest2.png")) {
    file.remove("x3ptest2.png")
  }
  if (file.exists("x3ptest2.svg")) {
    file.remove("x3ptest2.svg")
  }
  if (file.exists("x3ptest2.stl")) {
    file.remove("x3ptest2.stl")
  }
})

test_that("image_x3p works as expected", {
  expect_error(image_x3p("hello world"), ".x3p. .in. class.*x3p.* is not TRUE")
  image_x3p(x3ptest)
  rglwindowopen <- rgl::.check3d()
  # Check that a window is open
  expect_gte(rglwindowopen, 1)
  # If open, close it
  if (rglwindowopen) {
    rgl::rgl.close()
  }
  image_x3p(x3ptest, file = "x3ptest.png")
  expect_true(file.exists("x3ptest.png"))
  image_x3p(x3ptest, file = "x3ptest.svg")
  expect_true(file.exists("x3ptest.svg"))
  image_x3p(x3ptest, file = "x3ptest.stl")
  expect_true(file.exists("x3ptest.stl"))
  
  image_x3p(x3ptest, crosscut = 1)
  rglwindowopen <- rgl::.check3d()
  # Check that a window is open
  expect_gte(rglwindowopen, 1)
  # If open, close it
  if (rglwindowopen) {
    rgl::rgl.close()
  }
})

# test_that("image_x3p_grid works as expected", {
#   expect_error(image_x3p_grid("hello world"), ".x3p. .in. class.*x3p.* is not TRUE")
#   image_x3p_grid(x3ptest)
#   rglwindowopen <- rgl::.check3d()
#   # Check that a window is open
#   expect_gte(rglwindowopen, 1)
#   # If open, close it
#   if (rglwindowopen) {
#     rgl::rgl.close()
#   }
#   image_x3p_grid(x3ptest, file = "x3ptest2.png")
#   expect_true(file.exists("x3ptest2.png"))
#   image_x3p_grid(x3ptest, file = "x3ptest2.svg")
#   expect_true(file.exists("x3ptest2.svg"))
#   image_x3p_grid(x3ptest, file = "x3ptest2.stl")
#   expect_true(file.exists("x3ptest2.stl"))
# })

# test_that("image_x3p_crosscut works as expected", {
#   expect_error(image_x3p_crosscut("hello world"), ".x3p. .in. class.*x3p.* is not TRUE")
#   image_x3p_crosscut(x3ptest, crosscut = 3, crosscut_rad = 1)
#   rglwindowopen <- rgl::.check3d()
#   # Check that a window is open
#   expect_gte(rglwindowopen, 1)
#   # If open, close it
#   if (rglwindowopen) {
#     rgl::rgl.close()
#   }
#   image_x3p_crosscut(x3ptest, file = "x3ptest.png")
#   expect_true(file.exists("x3ptest.png"))
#   
#   image_x3p_crosscut(x3ptest, file = "x3ptest.stl")
#   expect_true(file.exists("x3ptest.stl"))
# })
