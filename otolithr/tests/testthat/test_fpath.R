library(tidyverse)
context("Manage path")

test_that("get_path() gives file dir correctly", {
  expect_setequal(get_path("../testdir1"),
                  c("../testdir1/FOO/data/foo.hdr",
                    "../testdir1/bar/cannot_read/cannot.hdr",
                    "../testdir1/bar/data/foo.hdr"))
  expect_setequal(get_path("../Spcs-dir"),
                  c("../Spcs-dir/othercruise/station/cannot_read/cannot.hdr",
                    "../Spcs-dir/othercruise/station/data/foo.hdr",
                    "../Spcs-dir/somecruise/mtfoo/cannot_read/cannot.hdr",
                    "../Spcs-dir/somecruise/mtfoo/data/foo.hdr"))
})


test_that("get_dir2load() gives paths end with 'data/xxx.hdr'", {
  paths <- get_path("../testdir1")
  expect_setequal(get_dir2load(paths),
                  c("../testdir1/FOO/data/foo.hdr",
                    "../testdir1/bar/data/foo.hdr"))
})
