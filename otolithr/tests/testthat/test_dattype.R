library(otolithr)
context("Control behavior according to dattype")

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

test_that("get_info.commercial() makes sample metadata correctly", {
  fname <- "Sardinops-melanostictus_20150827_Toyama_Nakase_005.hdr"
  info  <- get_info.commercial(fname)
  expect_is(info, "commercial")
  expect_equal(info$spcs, "Sardinops-melanostictus")
  expect_equal(info$date, "20150827")
  expect_equal(info$key1, "Toyama")
  expect_equal(info$key2, "Nakase")
  expect_equal(info$sampleno, "005")
})

test_that("get_info.reared() makes sample metadata correctly", {
  fname <- "Sardinops-melanostictus_20160810_temp14_01.hdr"
  info  <- get_info.reared(fname)
  expect_is(info, "reared")
  expect_equal(info$spcs, "Sardinops-melanostictus")
  expect_equal(info$date, "20160810")
  expect_equal(info$key1, "temp14")
  expect_true(is.na(info$key2))
  expect_equal(info$sampleno, "01")
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
