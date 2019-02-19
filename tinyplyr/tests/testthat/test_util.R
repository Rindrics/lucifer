# -------------------------------------------------------------------.
# This file is tangled from tinyplyr.org.                            |
# (https://github.com/smxshxishxad/tidyNAS/blob/master/tinyplyr.org) |
# Do not edit by hand.                                               |
# ------------------------------------------------------------------'
context("Util functions work correctly")

test_that("num2date () convert numdate from Excel correctly", {
  expect_equal(num2date(58), "1900-02-27")
  expect_equal(num2date(59), "1900-02-28")
  expect_error(num2date(60), "This date is not correct in Excel.")
  expect_equal(num2date(61), "1900-03-01")
  expect_equal(num2date(62), "1900-03-02")
})
