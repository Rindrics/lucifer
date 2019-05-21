context("Process vectors")

test_that("rep_na_rep() replace NAs", {
  expect_equal(rep_na_rep(c(NA, 1, 2, rep(NA, 3))),
               c(NA, 1, rep(2, 4)))
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
