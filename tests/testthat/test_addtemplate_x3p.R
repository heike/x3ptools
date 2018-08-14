context("addtemplate_x3p")

test_that("addtemplate_x3p works as expected", {
  set.seed(323523)
  dftest <- expand.grid(x = 1:6, y = 1:6) %>%
    as.data.frame() %>%
    dplyr::mutate(z = rnorm(36, 0, .1) + sqrt((x - 3.5)^2 + (y - 3.5)^2),
                  value = z)
  x3ptest <- df_to_x3p(dftest[,c(1, 2, 4)])
  x3ptest2 <- addtemplate_x3p(x3ptest, template = system.file("templateXML.xml", 
                                                              package = "x3ptools"))
  x3ptest3 <- addtemplate_x3p(x3ptest)
  x3ptest4 <- x3ptest
  x3ptest4$feature.info <- list("Some feature info here")
  x3ptest5 <- addtemplate_x3p(x3ptest4)
  
  expect_error(
    expect_equivalent(
      x3ptest$feature.info, 
      x3ptest2$feature.info)
  )
  expect_equivalent(x3ptest2, x3ptest3)
  expect_equivalent(x3ptest4$feature.info, x3ptest5$feature.info)
  expect_equivalent(x3ptest5$general.info, x3ptest3$general.info)
})
