context("Rebel prefectures using rebel()")

test_that("rebel_sheet() beat aomori data up", {
  fname <- "excels/aomori.xlsx"
  sheet <- "マイワシ"
  year  <- 2019
  y_regex <- paste0("^", year)
  maiwashi <- rebel_sheet(path = fname, sheet = sheet,
                          cluster = list(dir = "v",
                                         pos = 1,
                                         regex = "^年",
                                         offset = c(0, 0),
                                         ends = list(row = y_regex,
                                                     col = "12月"),
                                         info = list(key_offset = c(-2, 0),
                                                     key_dim = c(1, 1),
                                                     value_offset = c(-2, 1),
                                                     value_dim = c(1, 1),
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
                        cluster = list(dir = "v",
                                       pos = 1,
                                       regex = "^年",
                                       offset = c(0, 0),
                                       ends = list(row = y_regex,
                                                   col = "12月"),
                                       info = list(key_offset = c(-2, 0),
                                                   key_dim = c(1, 1),
                                                   value_offset = c(-2, 1),
                                                   value_dim = c(1, 1),
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
  fname <- "excels/iwate.xls"
  sheet <- "マイワシ"
  year  <- 2018
  row_regex <- paste0("^", year, "")

  maiwashi <- rebel_sheet(path = fname, sheet = sheet,
                          cluster = list(dir = "v",
                                         pos = 1,
                                         regex = ".+によるマイワシ.+",
                                         offset = c(2, 0),
                                         ends = list(row = row_regex,
                                                     col = "(1|１)(2|２)月$")),
                          row_type = "Y",
                          col_type = list(regex = "^(1|１)?.月$",
                                          newname = "month",
                                          varname = "catch"))

  expect_equal(unique(maiwashi$year), 1968:2018)
  expect_equal(unique(maiwashi$month), 1:12)
  expect_setequal(unique(maiwashi$catch), as.character(1:1224))
  expect_equal(nrow(maiwashi), 1224)
})

test_that("duplicated column and fisY", {
  fname <- "excels/saga.xls"
  year  <- 2016
  saga  <- fname %>%
    rebel(sheet_regex = "Sheet.",
          row_type = "fisY",
          col_type = list(regex = ".+月",
                          newname = "month",
                          varname = "catch"),
          cluster = list(dir = "v",
                         pos = 1,
                         regex = "年度",
                         offset = c(0, 0),
                         ends = list(row = as.character(year),
                                     col = "３月")),
          unfiscalize = c(month_start = 4,
                          rule = "tail")
          )
  expect_equal(colnames(saga),
               c("fisy", "年度.1", "fname", "sheet", "month", "catch", "year"))
  expect_equal(colnames(saga),
               c("fisy", "年度.1", "fname", "sheet", "month", "catch", "year"))
  expect_setequal(subset(saga, year == 1976)$catch,
                  as.character(c(10:21, 514:525)))

  expect_success(
    expect_error(fname %>%
                   rebel(sheet_regex = "Sheet.",
                         row_type = "fisY",
                         col_type = list(regex = ".+月",
                                         newname = "month",
                                         varname = "catch"),
                         cluster = list(dir = "v",
                                        pos = 1,
                                        regex = "年度",
                                        offset = c(0, 0),
                                        ends = list(row = as.character(year),
                                                    col = "３月"))),
                 "Use 'unfiscalize = c(month_start =, rule =)'", fixed = TRUE))
})

test_that("dim = c(1, 1) info in saga", {
  fname <- "excels/saga2.xlsx"
  year  <- 2018
  regex_rend <- Nippon::jyear(year + 1) %>%
    stringr::str_replace("平成", "H") %>%
    paste0("3月")

  saga <- fname %>%
    lucifer::rebel_sheet(
               sheet = "ﾏｲﾜｼ～ｳﾙﾒ",
               col_type = list(regex = "定置網|まき網|その他",
                               newname = "fishery",
                               varname = "catch"),
               cluster = list(dir = "v",
                              pos = 1,
                              regex = ".+漁業種類別月別漁獲量（玄海漁協魚市場）",
                              offset = c(1, 0),
                              ends = list(row = regex_rend,
                                          col = "その他"),
                              info = list(value_offset = c(-1, 0),
                                          value_dim = c(1, 1)))) %>%
    tidyr::separate(`NA`, sep = "年", into = c("year", "month")) %>%
    dplyr::mutate(year = stringr::str_replace(year, "H", "平成") %>%
                    Nippon::wareki2AD(),
                  month = lucifer::make_ascii(month, numerize = TRUE) %>%
                    as.integer(),
                  catch = as.numeric(catch))
  expect_setequal(colnames(saga),
                  c("year", "month", "key1",
                    "fname", "sheet", "fishery", "catch"))
  expect_equal(unique(saga$key1),
               c("マイワシ漁業種類別月別漁獲量（玄海漁協魚市場）",
                 "マアジ漁業種類別月別漁獲量（玄海漁協魚市場）",
                 "マサバ漁業種類別月別漁獲量（玄海漁協魚市場）"))
})


test_that("ehime", {
  fname <- "excels/ehime.xls"
  converted <- fname %>%
    lucifer::rebel(sheet_regex = "マサバ.+",
                   cluster = list(regex = "^年$",
                                  direction = "h",
                                  pos = 2,
                                  offset = c(0, 0),
                                  ends = list(row = "2019年",
                                              col = "マサバ")))
  expect_equal(converted$ｺﾞﾏｻﾊﾞ,
               seq(1, by = 2, length.out = nrow(converted)) %>%
                 as.character())
  expect_equal(converted$マサバ,
               seq(2, by = 2, length.out = nrow(converted)) %>%
                 as.character())
  expect_equal(nrow(converted), 28)
})
