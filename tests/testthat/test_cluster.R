context("Test extracting clustered data")

test_that("clusters distributed in row direction can be extracted", {
  data  <- data.frame(a = rep(c("foo", "bar", "baz", "bum"), 4),
                      b = 1:16, c = 11:26, d = 21:36, stringsAsFactors = FALSE)
  data2 <- extract_a_cluster(pos_key = 1, find_from = 1, direction = "row",
                             df = data, offset = c(0, 0),
                             ends = list(row = "bar", col = "21"))
  expect_equal(vectorize_row(data2, 1), c("foo", 1, 11, 21))
  expect_equal(data2$a, c("foo", "bar"))
  expect_equal(data2$b, 1:2)

  data2 <- extract_a_cluster(pos_key = 1, find_from = 1, direction = "row",
                             df = data, offset = c(1, 1),
                             ends = list(row = "3", col = "22"))
  expect_equal(data2$b, 2:3)
  expect_equal(data2$c, 12:13)

  data2 <- extract_a_cluster(pos_key = 1, find_from = 1, direction = "row",
                             df = data, offset = c(1, 1),
                             ends = list(row = "4", col = "22"))
  expect_equal(data2$b, 2:4)
  expect_equal(data2$c, 12:14)
  expect_equal(data2$d, 22:24)

  data2 <- extract_a_cluster(pos_key = 1, find_from = 1, direction = "row",
                             df = data, offset = c(2, 1),
                             ends = list(row = "4", col = "23"))
  expect_equal(data2$b, 3:4)
  expect_equal(data2$c, 13:14)
  expect_equal(data2$d, 23:24)

  data  <- data.frame(rbind(1:10, 11:20, 21:30, 31:40))
  data2 <- extract_a_cluster(pos_key = 1, find_from = 1, direction = "col",
                             df = data, offset = c(0, 0),
                             ends = list(row = "11", col = "2"))
  expect_equal(data2$X1, c(1, 11))
  expect_equal(data2$X2, c(2, 12))

  data2 <- extract_a_cluster(pos_key = 1, find_from = 1, direction = "col",
                             df = data, offset = c(1, 1),
                             ends = list(row = "32", col = "14"))
  expect_equal(data2$X2, c(12, 22, 32))
  expect_equal(data2$X3, c(13, 23, 33))
  expect_equal(data2$X4, c(14, 24, 34))

  data2 <- extract_a_cluster(pos_key = 1, find_from = 1, direction = "col",
                             df = data, offset = c(2, 1),
                             ends = list(row = "32", col = "24"))
  expect_equal(data2$X2, c(22, 32))
  expect_equal(data2$X3, c(23, 33))
  expect_equal(data2$X4, c(24, 34))
})

test_that("additional info can be appended to df", {
  data  <- load_alldata("excels/cluster_info.xlsx", sheet = "Sheet1")
  data2 <- extract_a_cluster(pos_key = 5, find_from = 1, direction = "row",
                             df = data, offset = c(0, 0),
                             ends = list(row = "5", col = "bar"),
                             info = list(key_offset = c(-4, 0),
                                         key_dim = c(4, 1),
                                         value_offset = c(-4, 1),
                                         value_dim = c(4, 1)))
  expect_equal(data2[-1, 1], as.character(1:5))
  expect_equal(data2[-1, 2], as.character(16:20))
  expect_equal(vectorize_row(data2, 1),
               c("foo", "bar", "this", "is", "a", "test"))
  expect_equal(vectorize_row(data2, 2), c("1", "16", "1", "2", "3", "4"))
})

test_that("clusters distributed in column direction can be extracted", {
  data  <- data.frame(rbind(rep(c("foo", "bar", "baz", "bum", "bup"), 2),
                            1:10, 11:20, 21:30, 31:40),
                      stringsAsFactors = FALSE)
  data2 <- unclusterize(data, regex = "foo",
                        direction = "h", pos = 1,
                        ends = list(row = "^1$|^6$", col = "bar"))
  expect_equal(data2[[1]][, 1], c("foo", 1))
  expect_equal(data2[[1]][, 2], c("bar", 2))
  expect_equal(data2[[2]][, 1], c("foo", 6))
  expect_equal(data2[[2]][, 2], c("bar", 7))

  data2 <- unclusterize(data, regex = "foo",
                         direction = "h", pos = 1,
                         offset = c(1, 1),
                         ends = list(row = "3.", col = "4|9"))
  expect_equal(data2[[1]][1, ] %>% as.numeric(), 2:4)
  expect_equal(data2[[1]][2, ] %>% as.numeric(), 12:14)
  expect_equal(data2[[2]][1, ] %>% as.numeric(), 7:9)
  expect_equal(data2[[2]][2, ] %>% as.numeric(), 17:19)


  data <- data.frame(a = rep(c("foo", "bar", "baz", "bum", "bup"), 2),
                     b = 1:10, c = 11:20,
                     stringsAsFactors = FALSE)

  data2 <- unclusterize(data, regex = "foo",
                        direction = "v", pos = 1,
                        ends = list(row = "baz", col = "11|16"))
  expect_equal(data2[[1]]$a, c("foo", "bar", "baz"))
  expect_equal(data2[[1]]$b, 1:3)
  expect_equal(data2[[1]]$c, 11:13)
  expect_equal(data2[[2]]$a, c("foo", "bar", "baz"))
  expect_equal(data2[[2]]$b, 6:8)
  expect_equal(data2[[2]]$c, 16:18)

  data2 <- unclusterize(data, regex = "foo", pos = 1,
                        direction = "v", offset = c(2, 1),
                        ends = list(row = "5|10", col = "13|18"))
  expect_equal(data2[[1]]$b, 3:5)
  expect_equal(data2[[1]]$c, 13:15)
  expect_equal(data2[[2]]$b, 8:10)
  expect_equal(data2[[2]]$c, 18:20)
})

test_that("'dim' can be controled by variable", {
  year <- 2019
  data <- load_alldata("excels/clustered.xlsx", sheet = "repeated")
  col2search <- 1
  data2 <- unclusterize(data, regex = "year", direction = "v",
                        pos = col2search,
                        offset = c(0, 0),
                        ends = list(row = as.character(year), col = "baz"),
                        info = list(key_offset = c(-4, 0),
                                    key_dim = c(4, 1),
                                    value_offset = c(-4, 1),
                                    value_dim = c(4, 1)))
  expect_equal(vectorize_row(data2[[1]], 1),
               c("year", "month", "foo", "bar", "baz",
                 "this", "is", "a", "info"))
  expect_equal(vectorize_row(data2[[1]], 2),
               as.character(c(2017, 1, 1:3, 1:4)))
  expect_equal(vectorize_row(data2[[2]], 2),
               as.character(c(2017, 1, 109:111, 5:8)))

  row2search <- 5
  data2 <- unclusterize(data, regex = "year", direction = "h",
                        pos = row2search,
                        offset = c(0, 0),
                        ends = list(row = as.character(year), col = "baz"),
                        info = list(value_offset = c(-4, 1),
                                    value_dim = c(4, 1)))
  expect_equal(data2[[1]][, 3],
               c("foo", seq(1, 106, by = 3) %>% as.character()))
  expect_equal(data2[[1]] %>%
                 vectorize_row(1),
               c("year", "month", "foo", "bar", "baz", paste0("key", 1:4)))
})

test_that("extract_culsters() throws an error", {
  col2search <- 1
  data <- load_alldata("excels/clustered.xlsx", sheet = "repeated")
  expect_success(
    expect_error(unclusterize(data, regex = "year", direction = "v",
                              pos = col2search,
                 offset = c(0, 0),
                 ends = list(row =  " ", col = "baz"),
                 info = list(key_offset = c(-4, 0),
                             key_dim = c(4, 1),
                             value_offset = c(-4, 1),
                             value_dim = c(4, 1))),
                 "Match failed. Re-consider regex"))
  expect_success(expect_error(
    unclusterize(data, regex = "year", regex = "year",
                 pos = col2search,
                 offset = c(0, 0),
                 ends = list(row = "ABCDEFG", col = "baz"),
                 info = list(offset = c(-4, 0),
                             dim = c(4, 2)))))
})

test_that("separated info", {
  data <- "excels/separated_info.xlsx" %>%
    rebel(sheet_regex = "Sheet.",
          cluster = list(dir = "v",
                         regex = "head1",
                              pos = 1,
                              offset = c(0, 0),
                              ends = list(row = "[0-9]+",
                                          col = "head4"),
                         info = list(key_offset = c(-1, 0),
                                     key_dim = c(1, 4),
                                     value_offset = c(1, 0),
                                     value_dim = c(1, 4))))
  expect_equal(colnames(data),
               c(paste0("head", 1:4), paste0("key", 1:4), "fname", "sheet"))
  expect_equal(data[1, ] %>% vectorize_row(),
               c(1, 31, 61, 91,
                 paste0("value", 1:4), "excels/separated_info.xlsx", "Sheet1"))
  expect_equal(dplyr::pull(data, 1), as.character(c(1:30, 121:150)))
})

test_that("offset(, -1) works correctly", {
  data <- "excels/hachinohe_ichiba.xls"  %>%
    lucifer::rebel_sheet(sheet = "1008_若鷹",
                         cluster = list(regex = "性別",
                                        dir = "v",
                                        pos = 2,
                                        offset = c(0, -1),
                                        ends = list(row = ".+",
                                                    col = "^採鱗$"),
                                        info = list(key_offset = c(-1, 1),
                                                    key_dim = c(1, 7),
                                                    value_offset = c(1, 1),
                                                    value_dim = c(1, 7))))
  expect_equal(colnames(data)[12], "漁獲年")
  expect_equal("肥満度" %in% colnames(data), FALSE)
})

test_that("position of scan starts correctly", {
  saba <- lucifer::rebel(path = "excels/miyagi.xlsx",
                         sheet_regex = ".+",
                         cluster = list(dir ="v",
                                        pos = 17,
                                        regex = "大中型|定置網|その他",
                                        offset = c(1,0),
                                        ends = list(row = "2019年$",
                                                    col = "１２月$")))
  expect_equal(nrow(saba), 25 * 3)
  expect_equal(ncol(saba), 15)
  expect_equal(vectorize_row(saba, 1),
               c("1995年", as.character(1201:1212),
                 "excels/miyagi.xlsx", "Sheet1"))
  expect_equal(vectorize_row(saba, 75),
               c("2019年", as.character(2089:2100),
                 "excels/miyagi.xlsx", "Sheet1"))

  maiwashi <- lucifer::rebel(path = "excels/miyagi.xlsx",
                             sheet_regex = ".+",
                             cluster = list(dir ="v",
                                            pos = 33,
                                            regex = "大中型|定置網|その他",
                                            offset = c(1,0),
                                            ends = list(row = "2019年$",
                                                        col = "１２月$")))
  expect_equal(nrow(maiwashi), 25 * 3)
  expect_equal(ncol(maiwashi), 15)
  maiwashi$年
  saba$年
  expect_equal(vectorize_row(maiwashi, 1),
               c("1995年", as.character(2401:2412),
                 "excels/miyagi.xlsx", "Sheet1"))
  expect_equal(vectorize_row(maiwashi, 75),
               c("2019年", as.character(3289:3300),
                 "excels/miyagi.xlsx", "Sheet1"))
  katakuchi <- lucifer::rebel(path = "excels/miyagi.xlsx",
                             sheet_regex = ".+",
                             cluster = list(dir ="v",
                                            pos = 49,
                                            regex = "大中型|定置網|その他",
                                            offset = c(1,0),
                                            ends = list(row = "2019年$",
                                                        col = "１２月$")))
  expect_equal(nrow(katakuchi), 25 * 3)
  expect_equal(ncol(katakuchi), 15)
  katakuchi$年
  saba$年
  expect_equal(vectorize_row(katakuchi, 1),
               c("1995年", as.character(3601:3612),
                 "excels/miyagi.xlsx", "Sheet1"))
  expect_equal(vectorize_row(katakuchi, 75),
               c("2019年", as.character(4489:4500),
                 "excels/miyagi.xlsx", "Sheet1"))
})
