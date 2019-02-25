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

test_that("is.jpdate() judge if given str is a jpdate", {
  expect_true(is.jpdate("H.29.8.22"))
  expect_true(is.jpdate("H29.8.22"))
  expect_false(is.jpdate("2000.8.22"))
})

test_that("split_jpdate() returns factors of jpdate", {
  split <- split_jpdate("H.29.08.22")
  expect_is(split, "list")
  expect_equal(split$year, 29)
  expect_equal(split$month, 8)
  expect_equal(split$day, 22)
})

test_that("jpdate2juliani() convert Japanese date to Julian day", {
  expect_equal(jpdate2juliani("H.29.8.22"), 42969)
  expect_equal(jpdate2juliani("H29.8.22"), 42969)
})
