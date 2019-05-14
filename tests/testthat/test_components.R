context("Process vectors")

test_that("rep_na_rep() replace NAs", {
  expect_equal(rep_na_rep(c(NA, 1, 2, rep(NA, 3))),
               c(NA, 1, rep(2, 4)))
})
