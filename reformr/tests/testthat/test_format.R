library(tidyverse)
context("Formatting loaded data")
param <- list()
indir <- "/Users/ahayashi/Documents/GitHub/tidyNAS/data/鮮魚関係/"
type  <- "sengyo"
spcs  <- "カタクチイワシ"
# get_filelist(param)
col_names <- c("sample.no", "bl_mm", "bw_g", "sex", "gw_g", "gsi", "otolith.taken", "original.fname", "original.sheetname")
# get_sheet2read(infile)
test_that("format() make datcols correctly", {
  expect_equal(length(colnames(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0125"))), length(col_names))
  expect_equal(length(colnames(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0325"))), length(col_names))
  expect_equal(length(colnames(format(paste0(indir, "鮮魚測定08/マイワシ.xls"), "0116"))), length(col_names))
  expect_equal(length(colnames(format(paste0(indir, "鮮魚測定08/マイワシ.xls"), "0117"))), length(col_names))
  expect_equal(colnames(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0125")), col_names, ignore.case = FALSE)
  expect_equal(colnames(format(paste0(indir, "鮮魚測定08/マイワシ.xls"), "0116")), col_names, ignore.case = FALSE)
  expect_equal(colnames(format(paste0(indir, "鮮魚測定08/マイワシ.xls"), "0117")), col_names, ignore.case = FALSE)
})

test_that("format() cleanses length and weight columns correctly", {
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0125")$sample.no), "integer")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0125")$bl_mm), "double")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0125")$bw_g), "double")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0125")$sex), "integer")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0125")$gw_g), "double")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0125")$gsi), "double")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0325")$sample.no), "integer")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0325")$bl_mm), "double")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0325")$bw_g), "double")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0325")$sex), "integer")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0325")$gw_g), "double")
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0325")$gsi), "double")
})
test_that("format() cleanses otolith and scale columns correctly", {
  expect_match(typeof(format(paste0(indir, "鮮魚測定06/カタクチイワシ.xls"), "0125")$otolith.taken), "integer")
})
