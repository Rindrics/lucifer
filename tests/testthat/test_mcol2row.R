context("Test mcol2row()")

test_that("mcol2row() gather YrowMcol data", {
  df <- data.frame(year = rep(2001:2019, each = 12),
                   month = rep(1:12, 19), catch = 1:(19 * 12)) %>%
    tidyr::spread(key = month, value = catch)
  df <- structure(df, row_type = "Y")
  expect_equal(mcol2row(df, varname = "catch"),
               data.frame(year = rep(2001:2019, each = 12),
                          month = rep(1:12, 19),
                          catch = 1:(19 * 12)))
  df_error <- structure(df, row_type = "foo")
  expect_error(mcol2row(df_error, varname = "catch"))

  df_noattr <- data.frame(year = rep(2001:2019, each = 12),
                   month = rep(1:12, 19), catch = 1:(19 * 12)) %>%
    tidyr::spread(key = month, value = catch)
  expect_error(mcol2row(df_noattr, varname = "catch"))
})

test_that("mcol2row() gather fisYrowMcol data", {
  df_fisy <- data.frame(year = rep(2001:2019, each = 12),
                        month = rep(c(4:12, 1:3), 19), catch = 1:(19 * 12)) %>%
    tidyr::spread(key = month, value = catch) %>%
    dplyr::select("year", as.character(4:12), as.character(1:3))
  df_fisy <- structure(df_fisy, row_type = "fisY")

  converted <- mcol2row(df_fisy, varname = "catch")
  expect_equal(converted$month, rep(c(4:12, 1:3), 19))
  expect_equal(converted$catch, 1:(19 * 12))
})

test_that("mcol2row() gather jYrowMcol data", {
  df <- data.frame(cbind(year = c(60:63, 1:15),
                         matrix(1:228, nrow = 19, ncol = 12, byrow = TRUE)))
  colnames(df) <- c("year", as.character(1:12))
  df <- structure(df, row_type = "jY")

  converted <- mcol2row(df, varname = "catch")
  expect_equal(converted$year, rep(1985:2003, each = 12))
  expect_equal(converted$month, rep(1:12, 19))
})
