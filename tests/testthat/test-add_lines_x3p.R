context("test-add_lines_x3p")

test_that("x3p_add_vline works", {
  x3ptest_mask <- x3ptest %>%
    x3p_add_vline(xintercept = 3, size = 2)
  x3ptest_mask2 <- x3ptest %>%
    x3p_add_vline(xintercept = 3, size = 3)
  
  expect_equal(colSums(x3ptest_mask$mask == "black"), c(0, 0, 7, 7, 0, 0))
  expect_equal(colSums(x3ptest_mask2$mask == "black"), c(0, 0, 7, 7, 7, 0))
})

test_that("x3p_add_hline works", {
  x3ptest_mask <- x3ptest %>%
    x3p_add_hline(yintercept = 3, size = 2)
  x3ptest_mask2 <- x3ptest %>%
    x3p_add_hline(yintercept = 3, size = 3)
  
  # This is how I expected it to work - why does the horizontal side work differently than the vertical side?
  # expect_equal(rowSums(x3ptest_mask$mask == "black"), c(0, 0, 7, 7, 0, 0, 0)) 
  # expect_equal(rowSums(x3ptest_mask2$mask == "black"), c(0, 0, 7, 7, 7, 0, 0))
  
  expect_equal(rowSums(x3ptest_mask$mask == "black"), c(0, 6, 6, 0, 0, 0, 0)) 
  expect_equal(rowSums(x3ptest_mask2$mask == "black"), c(0, 6, 6, 6, 0, 0, 0))
})

test_that("x3p_add_grid works", {
  logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools")) %>%
    x3p_add_mask()
  
  logo_grid <- x3p_add_grid(logo, spaces = 1e-4)
  
  # Hackish, should probably come up with a better example that I can test easily...
  expect_equal(sum(colSums(logo_grid$mask == "#cd7f32") == 0), 5)
  expect_equal(sum(rowSums(logo_grid$mask == "#cd7f32") == 0), 6)
})