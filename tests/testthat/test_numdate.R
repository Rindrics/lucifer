# This file is tangled from tinyplyr.org.
# (https://github.com/smxshxishxad/tinyplyr/tinyplyr.org)
# Edit that file.

context("Parse strings correctly")

test_that("num2datei () convert numdate from Excel correctly", {
  expect_equal(num2datei(58), "1900-02-27")
  expect_equal(num2datei(59), "1900-02-28")
  expect_error(num2datei(60), "This date is not correct in Excel.")
  expect_equal(num2datei(61), "1900-03-01")
  expect_equal(num2datei(62), "1900-03-02")
})


test_that("stdz_date() parse str into %Y-%m-%d format", {
  expect_equal(stdz_date("20180101", 2018), "2018-01-01")
  expect_equal(stdz_date("0101", 2018), "2018-01-01")
  expect_equal(stdz_date("43101", 2018), "2018-01-01")
  expect_equal(stdz_date("H.30.01.01", 2018), "2018-01-01")
  expect_equal(stdz_date("H30.1.1", 2018), "2018-01-01")
  expect_error(stdz_date("1", 2018),
               "Something's wrong with \"date\" data.", fix = TRUE)
})

test_that("num2date () convert numdate from Excel correctly", {
  expect_setequal(num2date(56:58), c("1900-02-25", "1900-02-26", "1900-02-27"))
})

test_that("is.jpdate() judge if given str is a jpdate", {
  expect_true(is.jpdate("H.29.8.22"))
  expect_true(is.jpdate("H29.8.22"))
  expect_false(is.jpdate("2000.8.22"))
})

test_that("split_jpdate() returns factors of jpdate", {
  split <- split_jpdate("H.29.08.22")
  expect_is(split, "list")
  expect_equal(split$era, "heisei")
  expect_equal(split$year, 29)
  expect_equal(split$month, 8)
  expect_equal(split$day, 22)
})

test_that("date2juliani() convert Japanese date to Julian day", {
  expect_equal(date2juliani("H.29.8.22"), 42969)
  expect_equal(date2juliani("H29.8.22"), 42969)
  expect_equal(date2juliani("2017.8.22"), 42969)
})


test_that("date2julian() convert Japanese date to Julian day", {
  expect_setequal(date2julian(c("H.29.8.22", "H.29.8.23")), c(42969, 42970))
  expect_setequal(date2julian(c("H.29.8.22", "H29-8-23")), c(42969, 42970))
  expect_setequal(date2julian(c("H.29.8.22", "H29-08-23")), c(42969, 42970))
})
