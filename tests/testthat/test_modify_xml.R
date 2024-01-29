context("xml description functions")


test_that("show_xml works as expected", {
  logo <- read_x3p(system.file("csafe-logo.x3p", package = "x3ptools"))

  expect_equal(
    x3p_show_xml(logo, "creator"),
    list(Creator = "Heike Hofmann, CSAFE")
  )
  expect_warning(
    x3p_show_xml(logo, "no_field_by_this_name"),
    "no fields containing .no_field_by_this_name. found"
  )
  expect_equal(
    x3p_show_xml(logo, 1),
    list(sizeY = 419)
  )
})


test_that("modify_xml works as expected", {
  logo <- read_x3p(system.file("csafe-logo.x3p", package = "x3ptools"))

  logo2 <- x3p_modify_xml(logo, "creator", "someone_else")
  expect_equal(
    x3p_show_xml(logo2, "creator"),
    list(Creator = "someone_else")
  )
  
  expect_error(
    x3p_modify_xml(logo, "x"),
    'argument "value" is missing, with no default'
  )

  expect_error(
    x3p_modify_xml(logo, "x", 1),
    "More than one element matching <x> found"
  )

  expect_error(
    x3p_modify_xml(logo, "nonsense_field", 1),
    "No element found matching <nonsense_field>"
  )
})
