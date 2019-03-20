context("test-x3p_mask_legend")

test_that("x3p_darker", {
  expect_error(x3p_darker())
  
  # With mask
  x3ptest %>% 
    x3p_add_mask() %>%
    image_x3p()
  
  lights <- rgl::rgl.ids(type = "lights")
  
  x3p_darker()
  
  lights2 <- rgl::rgl.ids(type = "lights")
  
  rgl::rgl.close()
  expect_lt(nrow(lights2), nrow(lights))
})

test_that("x3p_lighter", {
  expect_error(x3p_lighter())
  
  # With mask
  x3ptest %>% 
    x3p_add_mask() %>%
    image_x3p()
  
  lights <- rgl::rgl.ids(type = "lights")
  
  x3p_lighter()
  
  lights2 <- rgl::rgl.ids(type = "lights")
  
  rgl::rgl.close()
  
  expect_gt(nrow(lights2), nrow(lights))
})


