context("addtemplate_x3p")

test_that("addtemplate_x3p works as expected", {
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
