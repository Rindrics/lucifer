context("Rebel prefectures using rebel()")

test_that("rebel_sheet() beat aomori data up", {
  fname <- "aomori.xlsx"
  sheet <- "マイワシ"
  year  <- 2019
  y_regex <- paste0("^", year)
  maiwashi <- rebel_sheet(path = fname, sheet = sheet,
                        cluster = list(dir = "row",
                                       pos = 1,
                                       regex = "^年",
                                       offset = c(0, 0),
                                       ends = list(row = y_regex,
                                                   col = "12月"),
                                       info = list(offset = c(-2, 0),
                                                   dim = c(1, 2),
                                                   headerized = FALSE)),
                        row_type = "Y",
                        col_type = list(regex = "^1?[0-9]月?",
                                        newname = "month",
                                        varname = "catch"))
  expect_equal(dplyr::filter(maiwashi, year == 1997, month == 6,
                             漁法 == "定置網漁業（底建網含む）") %>%
               dplyr::select(catch) %>%
               vectorize_row(1),
               "666")
  expect_setequal(maiwashi$catch, as.character(1:936))
  expect_equal(unique(maiwashi$month), 1:12)
  expect_equal(unique(maiwashi$year), 1981:2019)
  expect_equal(unique(maiwashi$漁法), c("まき網漁業", "定置網漁業（底建網含む）"))
  expect_equal(unique(maiwashi$fname), fname)
  expect_equal(unique(maiwashi$sheet), sheet)

  sheet <- "カタクチ"
  katakuchi <- rebel_sheet(path = fname, sheet = sheet,
                        cluster = list(dir = "row",
                                       pos = 1,
                                       regex = "^年",
                                       offset = c(0, 0),
                                       ends = list(row = y_regex,
                                                   col = "12月"),
                                       info = list(offset = c(-2, 0),
                                                   dim = c(1, 2),
                                                   headerized = FALSE)),
                        row_type = "Y",
                        col_type = list(regex = "^1?[0-9]月?",
                                        newname = "month",
                                        varname = "catch"))
  expect_setequal(katakuchi$catch, as.character(1:936))
  expect_equal(unique(katakuchi$month), 1:12)
  expect_equal(unique(katakuchi$year), 1981:2019)
  expect_equal(unique(katakuchi$漁法), c("まき網漁業", "定置網漁業（底建網含む）"))
  expect_equal(unique(katakuchi$fname), fname)
  expect_equal(unique(katakuchi$sheet), sheet)
})

test_that("rebel_sheet() beat iwate data up", {
  fname <- "iwate.xls"
  sheet <- "マイワシ"
  year  <- 2018
  row_regex <- paste0("^", 2018)

  maiwashi <- rebel_sheet(path = fname, sheet = sheet,
                          cluster = list(dir = "row",
                                         pos = 1,
                                         regex = ".+によるマイワシ.+",
                                         offset = c(2, 0),
                                         ends = list(row = row_regex,
                                                     col = "(1|１)(2|２)月")),
                          row_type = "Y",
                          col_type = list(regex = "^1?[0-9]月",
                                          newname = "month",
                                          varname = "catch"))

  expect_equal(unique(maiwashi$year), 1968:2018)
  expect_equal(unique(maiwashi$month), 1:12)
  expect_setequal(unique(maiwashi$catch), as.character(1:1224))
})
