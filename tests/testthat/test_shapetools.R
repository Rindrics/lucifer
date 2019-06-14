context("Shaping data frame")

test_that("make_rect() makes weired df into rectangular shape", {
  data <- data.frame(A = 1:10, B = 11:20, C = 21:30)
  expect_equivalent(make_rect(data, range = "A1:C3"),
                    data.frame(A = 1:3, B = 11:13, C = 21:23))
  expect_equivalent(make_rect(data, range = "B2:C5"),
                    data.frame(B = 12:15, C = 22:25))
  expect_equivalent(make_rect(data, range = "A4:C2"),
                    data.frame(A = 2:4, B = 12:14, C = 22:24))
  expect_equivalent(make_rect(data, range = "C5:B1"),
                    data.frame(B = 11:15, C = 21:25))
})

test_that("unmerge_horiz() fill NAs of merged columns", {
  merged  <- load_alldata("merged.xlsx", sheet = "Sheet1")
  newname <- unmerge_horiz(merged, 1) %>%
    dplyr::slice(1) %>%
    unlist(use.names = FALSE)
  expect_equal(newname, c(NA, rep("A1", 3), rep("E1", 4)))
})

test_that("unmerge_vert() fill NAs of merged rows", {
  merged  <- load_alldata("merged.xlsx", sheet = "Sheet1")
  newname <- unmerge_vert(merged, 1) %>%
    dplyr::pull(1) %>%
    as.vector()
  expect_equal(newname, c(NA, rep("A2", 8), rep("A10", 6), rep("A16", 8)))
})
test_that("append_info() append information stored in a list to df as column", {

  info  <- list(foo = 1, bar = 2, baz = 3)
  df    <- data.frame(a = 1:10, b = 11:20)
  newdf <- append_info(df = df, info = info, headerized = TRUE)
  expect_equal(newdf$a, 1:10)
  expect_equal(newdf$b, 11:20)
  expect_equal(newdf$foo, rep(1, nrow(df)))
  expect_equal(newdf$bar, rep(2, nrow(df)))
  expect_equal(newdf$baz, rep(3, nrow(df)))
})

test_that("rm_matchrow() remove summary rows from df", {
  contami <- load_alldata("sumrow_contami.xlsx", sheet = "Sheet1")
  expect_equal(rm_matchrow(contami, key = "sum", colpos = 1, regex = FALSE) %>%
                 dplyr::pull(1),
               c(paste0("A", c(1:10, 12:20, 22:30))))
  expect_equal(rm_matchrow(contami, key = "s.m", colpos = 1, regex = TRUE) %>%
                 dplyr::pull(1),
               c(paste0("A", c(1:10, 12:20, 22:30))))
})

test_that("rm_matchcol() remove summary cols from df", {
  contami <- load_alldata("sumcol_contami.xlsx", sheet = "Sheet1")
  expect_equal(rm_matchcol(contami, key = "sum", rowpos = 1, regex = FALSE) %>%
                 dplyr::slice(1) %>%
                 unlist() %>%
                 as.vector(),
               c(paste0(LETTERS[1:8][c(-4, -7)], 1)))

  expect_equal(rm_matchcol(contami, key = "su.",
                         rowpos = 1, regex = TRUE) %>%
                 dplyr::slice(1) %>%
                 unlist() %>%
                 as.vector(),
               c(paste0(LETTERS[1:8][c(-4, -7)], 1)))
})

test_that("merge_colname() concatenates colnames in multiple rows", {
  multi  <- load_alldata("multi_colnames.xlsx", sheet = "Sheet1")
  expect_equal(merge_colname(multi, 1:3)[1, ] %>%
                 unlist() %>%
                 as.vector(),
               c("A1_A2_A3", "B1_B2_B3", "C1_C2_C3",
                 "D1_D2_D3", "E1_E2_E3", "F2_F2_F3"))
  expect_equal(merge_colname(multi, 1:3)[2, ] %>%
                 unlist() %>%
                 as.vector(),
               as.character(seq(4, 104, 20)))
  expect_equal(merge_colname(multi, 1)[1, ] %>%
                 unlist() %>%
                 as.vector(),
               c("A1", "B1", "C1",
                 "D1", "E1", "F2"))
  expect_equal(merge_colname(multi, 1:2)[1, ] %>%
                 unlist() %>%
                 as.vector(),
               c("A1_A2", "B1_B2", "C1_C2",
                 "D1_D2", "E1_E2", "F2_F2"))
})

test_that("make_ascii() convert full-width numbers into ASCII numbers", {
  zenkaku  <- load_alldata("fullwidth.xlsx", sheet = "Sheet1")
  expect_equal(make_ascii(zenkaku, row = 2) %>% vectorize_row(2),
               c("1", "11", "21", "311", "11", "1月", "1トン"))
  expect_equal(make_ascii(zenkaku, row = 2, numerize = TRUE) %>%
                 vectorize_row(2),
               c("1", "11", "21", "311", "11", "1", "1"))
  expect_equal(make_ascii(zenkaku, col = 1) %>% dplyr::pull(1),
               as.character(c("full", 1:5)))
  expect_equal(make_ascii(zenkaku[-1, ], col = 1, numerize = TRUE) %>%
                 dplyr::pull(1),
               1:5)
  expect_equal(make_ascii(zenkaku[-1, ], col = 2, numerize = TRUE) %>%
                 dplyr::pull(2),
               11:15)
  expect_equal(make_ascii(zenkaku[-1, ], col = 3, numerize = TRUE) %>%
                 dplyr::pull(3),
               21:25)
  expect_equal(make_ascii(zenkaku[-1, ], col = 4, numerize = TRUE) %>%
                 dplyr::pull(4),
               311:315)
  expect_equal(make_ascii(zenkaku[-1, ], col = 5, numerize = TRUE) %>%
                 dplyr::pull(5),
               11:15)
  expect_equal(make_ascii(zenkaku[-1, ], col = 6, numerize = TRUE) %>%
                 dplyr::pull(6),
               1:5)
  expect_equal(make_ascii(zenkaku[-1, ], col = 7, numerize = TRUE) %>%
                 dplyr::pull(7),
               1:5)
  expect_equal(make_ascii(zenkaku[-1, ], col = 6, numerize = FALSE) %>%
                 dplyr::pull(6),
               paste0(1:5, "月"))
  expect_equal(make_ascii(zenkaku[-1, ], col = 7, numerize = FALSE) %>%
                 dplyr::pull(7),
               paste0(1:5, "トン"))
})


test_that("headerize() change specific row into df header", {
  df <- data.frame(a = 1:10, b = 11:20, c = 21:30)
  expect_equal(colnames(headerize(df, 1)), c("1", "11", "21"))
  expect_equal(colnames(headerize(df, 2)), c("2", "12", "22"))
  expect_equal(colnames(headerize(df, 3)), c("3", "13", "23"))
})


test_that("rm_nacol() remove na columns", {
  df <- data.frame(a = 1:10, b = 11:20, c = 21:30)
  colnames(df) <- c("foo", NA, "baz")
  expect_equal(colnames(rm_nacols(df)), c("foo", "baz"))
})

test_that("gather_cols() gather YrowMcol data", {
  df <- data.frame(year = rep(2001:2019, each = 12),
                   month = rep(1:12, 19), catch = 1:(19 * 12)) %>%
    tidyr::spread(key = month, value = catch)
  converted <- gather_cols(df, regex = "[1-2]?[0-9]",
                          newname = "foo", varname = "bar")
  expect_setequal(converted$year, rep(2001:2019, each = 12))
  expect_setequal(converted$foo, as.character(rep(1:12, 19)))
  expect_setequal(converted$bar, 1:(19 * 12))
})


test_that("gather_cols() gather ycol data correctly", {
  df <- data.frame(month = 1:12, "2019" = 13:24, "2020" = 25:36) %>%
    dplyr::rename(`2019` = X2019,
                  `2020` = X2020)
  converted <- gather_cols(df, regex = "[0-9]{4}",
                          newname = "foo", varname = "bar")
  expect_equal(converted$foo, as.character(rep(2019:2020, each = 12)))
})

test_that("sheet2var() convert sheetname to variable", {
  data <- load_alldata("sheetname.xlsx", sheet = "foo") %>%
    headerize(1) %>%
    structure(sheetname = "foo")
  conv <- sheet2var(data, as = "misc")
  expect_equal(unique(conv$misc), "foo")
  conv <- sheet2var(data, as = "bar")
  expect_equal(unique(conv$bar), "foo")
})
