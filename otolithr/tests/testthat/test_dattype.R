library(tidyverse)
context("Controle behavior according to dattype")

test_that("set_type() gives type to fname correctly", {
  expect_is(set_type("foo.hdr", "survey"), "survey")
  expect_is(set_type("foo.hdr", "commercial"), "commercial")
  expect_error(set_type("foo.hdr", "bar"),
               "'type' must be one of 'survey', 'commercial', or 'reared'.")
})

test_that("get_info.survey() makes sample metadata correctly", {
  fname <- "Sardinops-melanostictus_YK1508_MT6_10.hdr"
  info  <- get_info.survey(fname)
  expect_is(info, "survey")
  expect_equal(info$spcs, "Sardinops-melanostictus")
  expect_equal(info$crs.name, "YK1508")
  expect_equal(info$stn, "MT6")
  expect_equal(info$sampleno, "10")
})


test_that("detect_type() detects dattype correctly", {
  path_survey     <- "foo/survey/bar.hdr"
  path_commercial <- "foo/commercial/bar.hdr"
  path_reared     <- "foo/reared/bar.hdr"
  path_error      <- "foo/bar.hdr"
  msg <- "Dir structure error. Run 'help(detect_type)'."
  expect_equal(detect_type(path_survey),"survey")
  expect_equal(detect_type(path_commercial),"commercial")
  expect_equal(detect_type(path_reared),"reared")
  expect_error(detect_type(path_error), msg, fix = TRUE)
})
