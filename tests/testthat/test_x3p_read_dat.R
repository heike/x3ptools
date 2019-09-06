context("x3p_read_dat")

write.table(dftest[, c(1, 2, 4)], col.names = F, row.names = F, file = "test.dat")
write.table(dftest[, c(1, 2, 4)], col.names = F, row.names = F, file = "test2.dat")

test_that("x3p_read_dat works", {
  expect_error(x3p_read_dat("file_doesnt_exist.dat"))
  expect_equivalent(x3ptest$surface.matrix, suppressMessages(x3p_read_dat("test.dat")$surface.matrix))
  expect_warning(suppressMessages(x3p_read_dat("test2.dat")$surface.matrix))
})
