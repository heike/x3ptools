context("x3p_trim_na")


test_that("x3p_trim_na works as expected", {
  logo_NA <- logo
  logo_NA$surface.matrix[logo_NA$surface.matrix == median(logo_NA$surface.matrix)] <- NA
  tmp <- x3p_trim_na(logo_NA)
  
  expect_equivalent(
    tmp %>% x3p_show_xml("Comment"),
    "image rendered from the CSAFE logo ; cropped from location ( 36 , 74 )"
  )
  
  na_rows <- apply(logo_NA$surface.matrix, MAR=1, FUN = function(x) {sum(is.na(x))})
  na_cols <- apply(logo_NA$surface.matrix, MAR=2, FUN = function(x) {sum(is.na(x))})
  idx_rows <- range(which(abs(diff(na_rows == 419))==1))
  idx_cols <- range(which(abs(diff(na_cols == 741))==1))
  
  expect_equivalent(
    dim(tmp$surface.matrix),
    c(diff(idx_rows), diff(idx_cols))
  )
  
  # missing values inside the file should not be removed
  logo_NA <- logo
  logo_NA$surface.matrix[,50] <- NA
  logo_NA$surface.matrix[50,] <- NA
  tmp <- x3p_trim_na(logo_NA)
  
  expect_equivalent(
    tmp$surface.matrix,
    logo_NA$surface.matrix
  )  

  # trimming a file without missing value should not do anything
  tmp <- logo %>% x3p_trim_na()
  expect_equivalent(
    logo$surface.matrix,
    tmp$surface.matrix
  )
  
})

