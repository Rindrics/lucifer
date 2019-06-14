context("Test component functions")

test_that("vectorize_row() convert row into vector", {
  df  <- data.frame(a = 1:10, b = 11:20)
  expect_equal(vectorize_row(df, 2), c(2, 12))
  expect_equal(vectorize_row(df, 3), c(3, 13))
})

test_that("rep_na_rep() replace NAs", {
  expect_equal(rep_na_rep(c(NA, 1, 2, rep(NA, 3))),
               c(NA, 1, rep(2, 4)))
})

test_that("list2df() expands list to data frame", {
  list <- list(foo = 3, bar = TRUE, baz = "baz")
  nrow <- 10
  df    <- list2df(list, nrow)
  expect_equal(df$foo, as.character(rep(3, nrow)))
  expect_equal(df$bar, as.character(rep(TRUE, nrow)))
  expect_equal(df$baz, as.character(rep("baz", nrow)))
})

test_that("paste_rows() pastes multiple cells on the given column", {
  df <- data.frame(a = c("foo", "bar", "baz", "bum"),
                   b = c("this", "is", "a", "test"),
                   c = 1:4)
  expect_equal(paste_rows(1, 1, df),
               "foo")
  expect_equal(paste_rows(1, 1:3, df),
               "foo_bar_baz")
  expect_equal(paste_rows(1, 1:4, df),
               "foo_bar_baz_bum")
  expect_equal(paste_rows(2, 1:4, df),
               "this_is_a_test")
  expect_equal(paste_rows(3, 1:4, df),
               "1_2_3_4")
})

test_that("make_hougan() creates vector houganshi", {
  str1 <- rep(1:10, 4) %>%
    replace(which(. %% 3  == 0), NA) %>%
    replace(which(. %% 5  == 0), "foo") %>%
    as.character()
  str2 <- c("いち", "に", "さん", "し",
            NA, "ろく", "なな", "はち", NA, "じゅう")
  str3 <- c("カ", NA, "タ", NA, "ク", NA, "チ", "イ", "ワ", "シ")
  expect_equal(make_hougan(str1), "12 4  78  12 4  78  12 4  78  12 4  78  ")
  expect_equal(make_hougan(str2), " に し      ")
  expect_equal(make_hougan(str3), "カ タ ク チイワシ")
})

test_that("locate_keys() locate positions of keys
 in row or column of given df", {
  data <- data.frame(a = c("foo", "bar", "baz", "bum"),
                     b = 1:4,
                     c = c("this", "is", "a", "test"),
                     stringsAsFactors = FALSE)
  expect_equal(locate_keys(df = data, regex = "foo", col = 1), 1)
  expect_equal(locate_keys(df = data, regex = "ba", col = 1), c(2, 3))
  expect_equal(locate_keys(df = data, regex = "foo", row = 1), 1)
  expect_equal(locate_keys(df = data, regex = "a", row = 3), c(1, 3))
  expect_error(locate_keys(df = data, regex = "a"),
               "Give either 'row' or 'col'")
  expect_error(locate_keys(df = data, regex = "a", row = 1, col = 1),
               "Give either 'row' or 'col'")
})

test_that("which_decrease() detect decrease in num vector", {
  expect_silent(which_decrease(1:3))
  expect_equal(which_decrease(1:3), 1:3, check.attributes = FALSE)
  expect_equal(which_decrease(c(1:3, 7:10)),
               c(1:3, 7:10), check.attributes = FALSE)
  expect_equal(which_decrease(c(5:3)), c(1, 2), check.attributes = FALSE)
  expect_equal(which_decrease(c(1:5, 4, 6:8)),
               5, check.attributes = FALSE)
  expect_message(which_decrease(c(1:5, 4, 6:8)),
                 "There is a decrease in given vector")
})

test_that("Stop if alert_skip() detect skip in num vector", {
  expect_silent(alert_skip(1:3))
  expect_equal(alert_skip(1:3), 1:3)
  expect_equal(alert_skip(c(5:3)), c(5:3))
  expect_error(alert_skip(c(1:3, 7:10)))
  expect_error(alert_skip(c(1:5, 4, 6:8)))
})

test_that("jpyr2ad() convert jpyear to A.D.", {
  expect_equal(jpyr2ad(1:10, "showa"), 1926:1935)
  expect_error(jpyr2ad(c(60:62, 1), "showa"))
  expect_equal(jpyr2ad(c(60:63, 1), "showa"), c(1985:1989))
})

test_that("locate_matchend() locates end of the repeted match", {
  str <- c("foo", rep("bar", 10), rep("baz", 10))
  regex <- "baz"
  expect_equal(locate_matchend(str, "bar"), 11)

  str <- rep(c("foo", "bar", "baz"), 5)
  regex <- "bar"
  expect_equal(locate_matchend(str, regex), 2)
})
