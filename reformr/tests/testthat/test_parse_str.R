context("Parse strings")

test_that("parse_year() puts year list correctly", {
  expect_match(parse_year("鮮魚測定11"), "2011")
  expect_match(parse_year("鮮魚測定99"), "2099")
  expect_equal(parse_year("aaaa99"), NA)
  expect_equal(parse_year("____99"), NA)
  expect_equal(parse_year("123499"), NA)
})

# test_that("get_date() parses dates correctly", {
#   expect_equal(get_date(2012, "0201"), as.Date("2012-02-01"))
#   expect_equal(get_date(2012, "0201(1)"), as.Date("2012-02-01"))
#   expect_equal(get_date(2017, "170201(1)"), as.Date("2017-02-01"))
#   expect_equal(get_date(2017, "170201(11)"), as.Date("2017-02-01"))
# })

test_that("get_spcsname() gets spcs name", {
  expect_equal(get_spcsname("カタクチイワシ.xls"), "カタクチイワシ")
  expect_equal(get_spcsname("カタクチ.xls"), "カタクチイワシ")
  expect_equal(get_spcsname("カタクチ.xls"), "カタクチイワシ")
  goodnames <- c("カタクチイワシ", "マイワシ", "マアジ", "ウルメイワシ")
  variants  <- c("カタクチイワシ", "カタクチ", "マイワシ", "マアジ",
                 "ウルメイワシ", "ウルメ")
  misc      <- c("測定", "鳥取", "XX丸", "")
  xtn       <- c(".xls", ".xlsx")
  fnames    <- expand.grid(variants, misc, xtn) %>%
    dplyr::transmute(fname = paste0(Var1, Var2, Var3)) %>%
    dplyr::pull(fname)
  expect_setequal(purrr::map(variants, get_spcsname) %>%
                    unlist() %>%
                    unique(),
                  goodnames)
})

test_that("filter_sheet() gets sheets with good name format", {
  expect_equal(filter_sheet("0201"), "0201")
  expect_equal(filter_sheet("0201(1)"), "0201(1)")
  expect_equal(filter_sheet("0201大"), "0201大")
  expect_equal(length(filter_sheet("体長")), 0)
  expect_equal(length(filter_sheet("Sheet1")), 0)
  sheets <- c("体長", "0125", "0204", "0207(1)", "0217", "0325", "0407",
              "0428", "0510", "0524", "0929", "Sheet2", "Sheet3")
  expect <- c("0125", "0204", "0207(1)", "0217", "0325", "0407",
              "0428", "0510", "0524", "0929")
  expect_setequal(filter_sheet(sheets), expect)
})

test_that("parse_date() makes date from sheetname vector", {
  expect_equal(parse_date(2012, "0125"), "2012-01-25")
  expect_equal(parse_date(2016, "0204"), "2016-02-04")
})
