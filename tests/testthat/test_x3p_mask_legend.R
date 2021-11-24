context("x3p_mask_legend")

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


test_that("x3p_add_legend", {
  expect_error(x3p_add_legend("x3p"))
  expect_error(x3p_add_legend(x3pbig)) 

  # With mask
  x3pbig2 %>%
    image_x3p()

  objs <- rgl::rgl.attrib.info(showAll = T)
  x3p_add_legend(x3pbig2)

  objs2 <- rgl::rgl.attrib.info(showAll = T)

  rgl::rgl.close()
  extra_stuff <- dplyr::anti_join(objs2, objs)
  expect_gt(nrow(extra_stuff), 0) # TODO: Actually test for background? Not sure how to get at that...


  # With mask
  x3pbig2 %>%
    image_x3p()

  objs <- rgl::rgl.attrib.info(showAll = T)
  x3p_add_legend(x3pbig2, colors = c("not actually in here" = "#FF0000"))

  objs2 <- rgl::rgl.attrib.info(showAll = T)

  rgl::rgl.close()
  extra_stuff <- dplyr::anti_join(objs2, objs)
  expect_gt(nrow(extra_stuff), 0) # TODO: Actually test for background? Not sure how to get at that...
})


test_that("x3p_mask_legend", {
  expect_error(x3p_mask_legend("x3p"))
  expect_null(x3p_mask_legend(x3ptest))
  x3pmask <- x3ptest %>% x3p_add_mask()
  expect_null(x3p_mask_legend(x3ptest))
  x3pbig2
})
