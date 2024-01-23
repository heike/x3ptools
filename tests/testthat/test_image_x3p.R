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
  image_x3p(x3ptest, size=60)
  rglwindowopen <- rgl::.check3d()
  # Check that a window is open
  expect_gte(rglwindowopen, 1)
  # If open, close it
  if (rglwindowopen) {
    rgl::close3d()
  }
  x3ptest2 <- x3ptest %>% x3p_add_grid(spaces = 2, size = 1, color = "black")
  image_x3p(x3ptest2, file = "x3ptest.png")
  expect_true(file.exists("x3ptest.png"))
  image_x3p(x3ptest, file = "x3ptest.svg")
  expect_true(file.exists("x3ptest.svg"))
  image_x3p(x3ptest, file = "x3ptest.stl")
  expect_true(file.exists("x3ptest.stl"))

#  expect_warning(image_x3p(x3ptest, crosscut = 1), "Use of crosscut is deprecated")
  rglwindowopen <- rgl::.check3d()
  # Check that a window is open
  expect_gte(rglwindowopen, 1)
  # If open, close it
  if (rglwindowopen) {
    rgl::close3d()
  }

  # With mask
  x3ptestmask <- x3ptest %>%
    x3p_add_mask()
  x3ptestmask %>% image_x3p()
  rglwindowopen <- rgl::.check3d()
  # Check that a window is open
  expect_gte(rglwindowopen, 1)
  # If open, close it
  if (rglwindowopen) {
    rgl::close3d()
  }

  # expect_warning(
  #   image_x3p(x3ptestmask, crosscut = 5),
  #   "(Use of crosscut is deprecated)|(Crosscut does not map to x3p file correctly)",
  #   all = T
  # )
  # Check that a window is open
  expect_gte(rglwindowopen, 1)
  # If open, close it
  if (rglwindowopen) {
    rgl::close3d()
  }
})

test_that("image.x3p works as expected", {
  image(x3ptest)
  yaxis <- par()$yaxp
  expect_equal(yaxis, c(1, 0, 5))
})
