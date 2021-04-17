context("df_to_x3p")

# Test missing values messages
dftest2 <- dftest[sample(1:42, size = 38), c(1, 2, 4)]


test_that("df_to_x3p works as expected", {
  suppressWarnings(
    expect_error(
      df_to_x3p(dftest[, 1:3]),
      "!is.null.dframe\\$value. is not TRUE"
    )
  )

  expect_silent(tmp <- df_to_x3p(dftest[, c(1, 2, 4, 5)]))
  expect_equivalent(
    tmp$surface.matrix,
    matrix(dplyr::arrange(dftest, desc(y))$value,
      byrow = F, nrow = 6
    )
  )
  expect_equivalent(tmp$header.info, list(sizeX = 6, sizeY = 7, incrementX = 1, incrementY = 1))

  expect_message(
    x3ptest2 <- df_to_x3p(dftest2),
    "dframe has missing values ... they will be expanded"
  )
})

test_that("x3p_to_df works as expected", {
  expect_silent(tmp <- x3p_to_df(x3ptest_mask))
  expect_equivalent(
    tmp %>%
      dplyr::mutate(x = x + 1, y = y + 1) %>%
      dplyr::arrange(y, x) %>%
      dplyr::select(-annotation),
    dftest[, c(1, 2, 4, 5)]
  )

  x3ptest2 <- x3ptest
  x3ptest2$header.info <- list(sizeX = NULL, sizeY = NULL, incrementX = NULL, incrementY = NULL)
  expect_warning(tmp2 <- x3p_to_df(x3ptest2))
  expect_equivalent(sort(tmp2$value), sort(dftest$value))


  x3ptest2 <- x3ptest
  x3ptest2$header.info <- list(
    sizeX = NULL, sizeY = NULL, incrementX = NULL, incrementY = NULL,
    num_obs_per_profile = 6, num_profiles = 7, profile_inc = 1, obs_inc = 1
  )
  expect_silent(tmp3 <- x3p_to_df(x3ptest2))
  expect_equivalent(sort(tmp3$value), sort(dftest$value))
})
