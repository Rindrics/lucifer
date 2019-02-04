context("Integrated test")

test_that("make_outfname() creates out fname", {
  path   <- "../../../data.git/鮮魚関係/鮮魚測定06/カタクチイワシ.xls"
  sheets <- c("0125", "0204", "0207(1)", "0217", "0325", "0407", "0428",
              "0510", "0524", "0929")
  mmdd   <- purrr::map(sheets, filter_sheet) %>% unlist()
  year   <- 2006
  spcs   <- "カタクチイワシ"
  xtn    <- ".csv"
  expect <- paste0(year, "_", spcs, "_", mmdd, xtn)
  expect_equal(make_outfname(path), expect)
})

# test_that("get_sheet2read() puts sheets to read", {
#   expect_match(get_sheet2read(infile06), "0125", all = FALSE)
#   expect_match(get_sheet2read(infile06), "0207", all = FALSE)
#   expect_equal(length(get_sheet2read(infile06)), 10)
#   expect_equal(length(get_sheet2read(infile17)), 0)
# })

# test_that("make_datlist make data list correctly", {
#   expect_match(make_datlist(indir, type), "鮮魚測定06", all = FALSE)
#   expect_match(make_datlist(indir, type), "鮮魚測定07", all = FALSE)
# })

# test_that("get_filelist() puts species file list correctly", {
#   expect_match(get_filelist(indir, "カタクチイワシ"), "カタクチイワシ.xls", all = FALSE)
#   expect_match(get_filelist(indir, "マイワシ"), "マイワシ.xls", all = FALSE)
#   expect_error(get_filelist(indir), "argument \"spcs\" is missing, with no default", fixed = TRUE)
#   expect_error(get_filelist(indir, NA), "Give me Japanese species name", fixed = TRUE)
# })
