library(otolithr)
context("Manage path")

test_that("get_path() gives file dir correctly", {
  expect_setequal(
    get_path("../Genus-spcs"),
    c("../Genus-spcs/commercial/foo/cannot_read/cannot.hdr",
      "../Genus-spcs/commercial/foo/data/Sardinops-melanostictus_foo_bar_01.hdr",
      "../Genus-spcs/commercial/foo/data/Sardinops-melanostictus_foo_bar_10.hdr",
      "../Genus-spcs/reared/foo/cannot_read/cannot.hdr",
      "../Genus-spcs/reared/foo/data/Sardinops-melanostictus_foo_bar_01.hdr",
      "../Genus-spcs/survey/mtfoo/cannot_read/cannot.hdr",
      "../Genus-spcs/survey/mtfoo/data/Sardinops-melanostictus_foo_MT01_01.hdr"))
})


test_that("get_dir2load() gives paths end with 'data/xxx.hdr'", {
  paths <- get_path("../Genus-spcs")
  expect_setequal(
    get_dir2load(paths),
    c("../Genus-spcs/commercial/foo/data/Sardinops-melanostictus_foo_bar_01.hdr",
      "../Genus-spcs/commercial/foo/data/Sardinops-melanostictus_foo_bar_10.hdr",
      "../Genus-spcs/reared/foo/data/Sardinops-melanostictus_foo_bar_01.hdr",
      "../Genus-spcs/survey/mtfoo/data/Sardinops-melanostictus_foo_MT01_01.hdr"))
})

test_that("fullpath2fname() extracts only fname from full path", {
  path <- get_path("../Genus-spcs")[1]
  expect_equal(fullpath2fname(path), "cannot.hdr")
})
