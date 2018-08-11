context("df_to_x3p")

set.seed(323523)
dftest <- expand.grid(x = 1:6, y = 1:6) %>%
  as.data.frame() %>%
  dplyr::mutate(z = rnorm(36, 0, .1) + sqrt((x - 3.5)^2 + (y - 3.5)^2),
                value = z)

# Test missing values messages
# dftest2 <- dftest[sample(1:36, size = 34),]


test_that("df_to_x3p works as expected", {
  suppressWarnings(
    expect_error(df_to_x3p(dftest[,1:3]),
               "!is.null.dframe\\$value. is not TRUE"))

  expect_silent(tmp <- df_to_x3p(dftest[,c(1, 2, 4)]))
  expect_equivalent(tmp$surface.matrix,
                    matrix(dplyr::arrange(dftest, desc(y))$value,
                           byrow = F, nrow = 6))
  expect_equivalent(tmp$header.info, list(sizeX = 6, sizeY = 6, incrementX = 1, incrementY = 1))
})
