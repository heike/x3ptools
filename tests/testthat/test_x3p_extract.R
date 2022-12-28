context("x3p_extract")


test_that("x3p_extract works as expected", {
  expect_equivalent(
    x3ptest, # no mask, so nothing should happen
    x3ptest %>% x3p_extract()
  )
  
  x3ptest_extract <- x3ptest_mask %>% x3p_extract("#ffff00") 
  
  expect_message(
    x3p_cmp <- dftest %>% 
      dplyr::filter(mask == "#ffff00") %>% df_to_x3p(),
    "dframe has missing values ... they will be expanded"
  )
  
  expect_equivalent(
    x3ptest_extract$surface.matrix,
    x3p_cmp$surface.matrix
  )
})

