context("Parsing file name")
indir   <- "./data/調査船・冷凍サンプル（いわし・むろあじ・さば）"
n_files <- length(list.files(indir, pattern = ".+[カタクチ|マイワシ|マアジ].+測定.*\\.xls",
                             recursive = TRUE))
test_that("Parse year", {
  expect_identical(length(get_year_survey(indir)), n_files)
  expect_true(is.numeric(get_year_survey(indir)))
})

test_that("Parse filename to read", {
  expect_identical(length(list.files(indir, pattern = ".*[カタクチ].*測定.*\\.xls",
                                     recursive = TRUE, full.names = TRUE)),
                   length(get_filelist_survey(indir, "カタクチイワシ")))
})
