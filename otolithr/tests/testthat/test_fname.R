library(tidyverse)
context("Handle file name")

test_that("split_fname() split fname correctly", {
  fname <- "Sardinops-melanostictus_YK1508_MT6_10.hdr"
  split <- split_fname(fname)
  expect_equal(split[1], "Sardinops-melanostictus")
  expect_equal(split[2], "YK1508")
  expect_equal(split[3], "MT6")
  expect_equal(split[4], "10.hdr")
})

test_that("rm_extension() removes file extension correctly", {
  expect_equal(rm_extension("foo.hdr", ".hdr"), "foo")
  expect_equal(rm_extension("bar.txt", ".txt"), "bar")
  expect_equal(rm_extension("barhdr", ".hdr"), "barhdr")
  expect_equal(rm_extension("foo.hdr"), "foo")
  expect_error(rm_extension("foo.hdr", "hdr"),
               "'extension' must begin with '.'")
})

test_that("detect_var() detects given variable in given vector", {
  fname <- "Sardinops-melanostictus_YK1808_MT01_005.hdr"
  expect_equal(xtract_var(fname, "spcsname"), "Sardinops-melanostictus")
  expect_equal(xtract_var(fname, "cruise")  , "YK1808")
  expect_equal(xtract_var(fname, "stn")     , "MT01")
  expect_equal(xtract_var(fname, "sampleno"), "005")

  fname <- "Sardinops-melanostictus_20150827_Toyama_Nakase_005.hdr"
  expect_equal(xtract_var(fname, "spcsname"), "Sardinops-melanostictus")
  expect_equal(xtract_var(fname, "date")    , "20150827")
  expect_equal(xtract_var(fname, "key1")    , "Toyama")
  expect_equal(xtract_var(fname, "key2")    , "Nakase")
  expect_equal(xtract_var(fname, "sampleno"), "005")

  fname <- "Sardinops-melanostictus_20150827_Toyama_005.hdr"
  expect_equal(xtract_var(fname, "spcsname"), "Sardinops-melanostictus")
  expect_equal(xtract_var(fname, "date")    , "20150827")
  expect_equal(xtract_var(fname, "key1")    , "Toyama")
  expect_true(is.na(xtract_var(fname, "key2")))
  expect_equal(xtract_var(fname, "sampleno"), "005")
})
