library(tidyverse)
context("Parse file and sheet names")
indir <- "/Users/ahayashi/Documents/GitHub/tidyNAS/data/鮮魚関係"
type  <- "sengyo"
test_that("make_datlist make data list correctly", {
  expect_match(make_datlist(indir, type), "鮮魚測定06", all = FALSE)
  expect_match(make_datlist(indir, type), "鮮魚測定07", all = FALSE)
})

datlist <- make_datlist(indir, type)

test_that("parse_year() puts year list correctly", {
  expect_match(parse_year(datlist), "2008", all = FALSE)
  expect_match(parse_year(datlist), "2009", all = FALSE)
  expect_match(parse_year("鮮魚測定11"), "2011")
  expect_match(parse_year("鮮魚測定99"), "2099")
  expect_match(parse_year("aaaa99"), "2099")
  expect_match(parse_year("____99"), "2099")
  expect_match(parse_year("123499"), "2099")
})

test_that("get_filelist() puts species file list correctly", {
  expect_match(get_filelist(indir, "カタクチイワシ"), "カタクチイワシ.xls", all = FALSE)
  expect_match(get_filelist(indir, "マイワシ"), "マイワシ.xls", all = FALSE)
  expect_error(get_filelist(indir), "argument \"spcs\" is missing, with no default", fixed = TRUE)
  expect_error(get_filelist(indir, NA), "Give me Japanese species name", fixed = TRUE)
})

infile06 <- "/Users/ahayashi/Documents/GitHub/tidyNAS/data/鮮魚関係/鮮魚測定06/カタクチイワシ.xls"
infile17 <- "/Users/ahayashi/Documents/GitHub/tidyNAS/data/鮮魚関係/鮮魚測定17/カタクチイワシ.xls"

test_that("get_sheet2read() puts sheets to read", {
  expect_match(get_sheet2read(infile06), "0125", all = FALSE)
  expect_match(get_sheet2read(infile06), "0207", all = FALSE)
  expect_equal(length(get_sheet2read(infile06)), 10)
  expect_equal(length(get_sheet2read(infile17)), 0)
})

test_that("get_date() parses dates correctly", {
  expect_equal(get_date(2012, "0201"), as.Date("2012-02-01"))
  expect_equal(get_date(2012, "0201(1)"), as.Date("2012-02-01"))
  expect_equal(get_date(2017, "170201(1)"), as.Date("2017-02-01"))
  expect_equal(get_date(2017, "170201(11)"), as.Date("2017-02-01"))
})
