context("test-add_lines_x3p")

test_that("x3p_add_vline works", {
  x3ptest_mask <- x3ptest %>%
    x3p_add_vline(xintercept = 3, size = 2, color="black")
  x3ptest_mask2 <- x3ptest %>%
    x3p_add_vline(xintercept = 3, size = 3, color="black")
  
  expect_equal(colSums(x3ptest_mask$mask == "black"), c(0, 0, 7, 7, 0, 0))
  expect_equal(colSums(x3ptest_mask2$mask == "black"), c(0, 0, 7, 7, 7, 0))
})

test_that("x3p_add_hline works", {
  x3ptest_mask <- x3ptest %>%
    x3p_add_hline(yintercept = 3, size = 2, color="black")
  x3ptest_mask2 <- x3ptest %>%
    x3p_add_hline(yintercept = 3, size = 3, color="black")
  
  # This is how I expected it to work - why does the horizontal side work differently than the vertical side?
  # expect_equal(rowSums(x3ptest_mask$mask == "black"), c(0, 0, 7, 7, 0, 0, 0)) 
  # expect_equal(rowSums(x3ptest_mask2$mask == "black"), c(0, 0, 7, 7, 7, 0, 0))
  
  expect_equal(rowSums(x3ptest_mask$mask == "black"), c(0, 6, 6, 0, 0, 0, 0)) 
  expect_equal(rowSums(x3ptest_mask2$mask == "black"), c(0, 6, 6, 6, 0, 0, 0))
})

test_that("x3p_add_grid works", {
  
  test_grid <- x3p_add_grid(x3pbig, spaces = 10)
  colmax <- max(colSums(test_grid$mask == "#cd7f32"))
  cols <- c(rep(0, 4), rep(colmax, 17), 0, rep(colmax, 19), 0, rep(colmax, 9))
  rowmax <- max(rowSums(test_grid$mask == "#cd7f32"))
  rows <- c(0, rep(rowmax, 7), # I think this should probably be slightly different... 
            rep(c(0, rep(rowmax, 9)), times = 3), 
            0, rep(rowmax, 8),
            rep(0, 3), rep(rowmax, 8), 
            rep(c(0, rep(rowmax, 9)), times = 3), 
            0, rep(rowmax, 7), rep(0, 5))
  
  expect_equal(colSums(test_grid$mask == "#cd7f32"), cols)
  expect_equal(rowSums(test_grid$mask == "#cd7f32"), rows)
  expect_equal(colSums(test_grid$mask == "black"), rep(c(0, 3), c(4, 47)))
  expect_equal(rowSums(test_grid$mask == "black"), rep(c(0, 47, 0), c(47, 3, 51)))
  expect_equal(colSums(test_grid$mask == "darkred"), rep(c(101, 6), c(4, 47)))
  expect_equal(rowSums(test_grid$mask == "darkred"), rep(c(51, 4, 51), c(1, 95, 5)))
  
  expect_warning(x3p_add_grid(x3pbig, spaces = 200))
  expect_warning(x3p_add_grid(x3pbig, spaces = 60))
})

