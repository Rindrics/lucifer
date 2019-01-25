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

test_that("get_info.survey() makes sample metadata correctly", {
  fname <- "Sardinops-melanostictus_YK1508_MT6_10.hdr"
  info  <- get_info.survey(fname)
  expect_is(info, "survey")
  expect_equal(info$spcs, "Sardinops-melanostictus")
  expect_equal(info$crs.name, "YK1508")
  expect_equal(info$stn, "MT6")
  expect_equal(info$sampleno, "10")
})
