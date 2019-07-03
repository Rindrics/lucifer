context("Fights using multitools")

test_that("Fight with maiwashi sheet of aomori catch data", {
  df <- load_alldata("aomori.xlsx", sheet = "マイワシ")
  converted <- df %>%
    extract_clusters(regex = "^年", col = 1,
                     offset = c(0, 0), ends = list(row = "^2019", col = "12月"),
                     info = list(offset = c(-2, 0),
                                 dim = c(1, 2))) %>%
    lapply(headerize, row = 1) %>%
    purrr::invoke(rbind, .) %>%
    rm_nacols() %>%
    gather_cols(regex = "1?[0-9]月",
                newname = "month", varname = "catch")
  expect_equal(colnames(converted), c("年／月", "漁法", "month", "catch"))
  expect_equal(unique(converted$漁法), c("まき網漁業", "定置網漁業（底建網含む）"))
  expect_equal(unique(converted$month), paste0(1:12, "月"))
  expect_setequal(converted$catch, as.character(seq(1, 39 * 24)))
})

test_that("Fight with katakuchi sheet of aomori catch data", {
  df <- load_alldata("aomori.xlsx", sheet = "カタクチ")
  converted <- df %>%
    extract_clusters(regex = "^年", col = 1,
                     offset = c(0, 0), ends = list(row = "^2019", col = "12月"),
                     info = list(offset = c(-2, 0),
                                 dim = c(1, 2))) %>%
    lapply(headerize, row = 1) %>%
    purrr::invoke(rbind, .) %>%
    rm_nacols() %>%
    gather_cols(regex = "1?[0-9]月",
                newname = "month", varname = "catch")
  expect_equal(colnames(converted), c("年／月", "漁法", "month", "catch"))
  expect_equal(unique(converted$漁法), c("まき網漁業", "定置網漁業（底建網含む）"))
  expect_equal(unique(converted$month), paste0(1:12, "月"))
  expect_setequal(converted$catch, as.character(seq(1, 39 * 24)))
})

test_that("Fight with 'masabahi' sheet of aomori catch data", {
  df <- load_alldata("aomori.xlsx", sheet = "マサバ比  ")
  hachinohe <- df %>%
    extract_clusters(regex = "八戸", col = 1,
                     offset = c(1, 1),
                     ends = list(row = "[0-9]", col = "ゴマサバ")) %>%
    headerize(1)
  expect_equal(colnames(hachinohe), c("年月日", "マサバ", "ゴマサバ"))
  expect_equal(dplyr::pull(hachinohe, 1),
               as.character(c(43305, 43315, 43334, 43343,
                              43383, 43410, 43431, 43452, 43460)))
  expect_equal(dplyr::pull(hachinohe, 2), as.character(1:9))
  expect_equal(dplyr::pull(hachinohe, 3), as.character(99:91))

  tairadate <- df %>%
    extract_clusters(regex = "平舘", col = 1,
                     offset = c(1, 1),
                     ends = list(row = "[0-9]", col = "ゴマサバ")) %>%
    headerize(1)

  expect_equal(colnames(tairadate), c("月漁獲量", "マサバ", "ゴマサバ"))
  expect_equal(dplyr::pull(tairadate, 1), as.character(c(1:12, 1, 2)))
  expect_equal(dplyr::pull(tairadate, 2), as.character(1:14))
  expect_equal(dplyr::pull(tairadate, 3), as.character(99:86))
})

test_that("Fight with data from hachinohe ichiba", {
  fname <- "hachinohe_ichiba.xls"
  data <- load_alldata(fname, sheet = "0613") %>%
    extract_clusters(regex = "標本番号", col = 1,
                     offset = c(-1, 0),
                     ends = list(row = "[0-9]+", col = "採鱗")) %>%
    headerize(1) %>%
    data.frame()

  expect_equal(colnames(data)[1:5],
               c("標本番号", "性別", "年齢", "体長.mm.", "体重.g."))
  expect_equal(vectorize_row(data, 1),
               c("1", "sex1", "-1", "bl1", "bw1", "gw1",
                 "-1", "-1", "-1", NA, NA))
})

test_that("Fight with 'maiwashi' sheet of iwate data", {
  year <- 2018
  row_regex <- paste0("^", year)
  df <- load_alldata("iwate.xls", sheet = "マイワシ")

  maiwashi <- df %>%
    extract_clusters(regex = ".+によるマイワシ.+",
                     col = 1,
                     offset = c(2, 0),
                     ends = list(row = row_regex, col = "(1|１)(2|２)月")) %>%
    lapply(make_ascii, row = 1) %>%
    lapply(headerize, row = 1) %>%
    purrr::invoke(rbind, .) %>%
    gather_cols(regex = "1?[0-9]月",
                newname = "month", varname = "catch") %>%
    dplyr::rename(year = `NA`)
  head(maiwashi)

  expect_equal(unique(maiwashi$year), as.character(1968:2018))
  expect_equal(unique(maiwashi$month), paste0(1:12, "月"))
  expect_setequal(unique(maiwashi$catch), as.character(1:1224))
})