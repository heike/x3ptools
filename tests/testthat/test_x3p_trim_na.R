context("x3p_trim_na")


test_that("x3p_trim_na works as expected", {
  expect_silent(tmp <- x3ptest_mask %>% x3p_crop(width=4,height=4))

  logo_NA <- logo
  logo_NA$surface.matrix[logo_NA$surface.matrix == median(logo_NA$surface.matrix)] <- NA
  tmp <- x3p_trim_na(logo_NA)
  
  expect_equivalent(
    tmp %>% x3p_show_xml("Comment"),
    "image rendered from the CSAFE logo ; cropped from location ( 36 , 73 )"
  )
  
  na_rows <- apply(logo_NA$surface.matrix, MAR=1, FUN = function(x) {sum(is.na(x))})
  na_cols <- apply(logo_NA$surface.matrix, MAR=2, FUN = function(x) {sum(is.na(x))})
  idx_rows <- range(which(abs(diff(na_rows == 419))==1))
  idx_cols <- range(which(abs(diff(na_cols == 741))==1))
  
  expect_equivalent(
    dim(tmp$surface.matrix),
    c(diff(idx_rows)-1, diff(idx_cols)-1)
  )
})

