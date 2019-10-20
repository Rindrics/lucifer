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
  contami <- load_alldata("excels/sumrow_contami.xlsx", sheet = "Sheet1")
  expect_equal(rm_matchrow(contami, key = "sum", colpos = 1, regex = FALSE) %>%
                 dplyr::pull(1),
               c(paste0("A", c(1:10, 12:20, 22:30))))
  expect_equal(rm_matchrow(contami, key = "s.m", colpos = 1, regex = TRUE) %>%
                 dplyr::pull(1),
               c(paste0("A", c(1:10, 12:20, 22:30))))
})

test_that("rm_matchcol() remove summary cols from df", {
  contami <- load_alldata("excels/sumcol_contami.xlsx", sheet = "Sheet1")
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
  multi  <- load_alldata("excels/multi_colnames.xlsx", sheet = "Sheet1")
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
  zenkaku  <- load_alldata("excels/fullwidth.xlsx", sheet = "Sheet1")
  expect_equal(make_ascii(zenkaku, row = 2) %>% vectorize_row(2),
               c("1", "11", "21", "311", "11", "1月", "1トン"))
  expect_equal(make_ascii(zenkaku, row = 2, numerize = TRUE) %>%
                 vectorize_row(2),
               c("1", "11", "21", "311", "11", "1", "1"))
  expect_equal(make_ascii(zenkaku, col = 1) %>% dplyr::pull(1),
               as.character(c("full", 1:5)))
  expect_equal(make_ascii(zenkaku, col = 1, numerize = TRUE) %>%
                 dplyr::pull(1),
               c("full", as.character(1:5)))
  expect_equal(make_ascii(zenkaku, col = 2, numerize = TRUE) %>%
                 dplyr::pull(2),
               c("full  half", as.character(11:15)))
  expect_equal(make_ascii(zenkaku, col = 3, numerize = TRUE) %>%
                 dplyr::pull(3),
               c("half  full", as.character(21:25)))
  expect_equal(make_ascii(zenkaku, col = 4, numerize = TRUE) %>%
                 dplyr::pull(4),
               c("full-half-full", as.character(311:315)))
  expect_equal(make_ascii(zenkaku, col = 5, numerize = TRUE) %>%
                 dplyr::pull(5),
               c("full-full", as.character(11:15)))
  expect_equal(make_ascii(zenkaku, col = 6, numerize = TRUE) %>%
                 dplyr::pull(6),
               c("month", as.character(1:5)))
  expect_equal(make_ascii(zenkaku, col = 7, numerize = TRUE) %>%
                 dplyr::pull(7),
               c("ton", as.character(1:5)))
  expect_equal(make_ascii(zenkaku, col = 6, numerize = FALSE) %>%
                 dplyr::pull(6),
               c("month", paste0(1:5, "月")))
  expect_equal(make_ascii(zenkaku, col = 7, numerize = FALSE) %>%
                 dplyr::pull(7),
               c("ton", paste0(1:5, "トン")))
})

test_that("make_ascii() handle df with NA", {
  zenkaku  <- load_alldata("excels/fullwidth.xlsx", sheet = "Sheet1")
  zenkaku[1, 1] <- NA
  expect_equal(make_ascii(zenkaku, row = 1, headerized = FALSE) %>%
               vectorize_row(1),
               c("NA", "full  half", "half  full",
                 "full-half-full", "full-full", "month", "ton"))
})

test_that("make_ascii() handle headerized df", {
  zenkaku  <- load_alldata("excels/fullwidth.xlsx", sheet = "Sheet1") %>%
    headerize(1)
  expect_equal(make_ascii(zenkaku, row = 1, headerized = TRUE) %>%
                 vectorize_row(1),
               c("1", "11", "21", "311", "11", "1月", "1トン"))
})

test_that("make_ascii() throws an error", {
  data  <- data.frame(a = 1:3, b = 4:6)
  expect_error(make_ascii(data, headerized = TRUE))
})

test_that("make_ascii() handlez vector", {
  vec  <- c("１", "２", "３")
  expect_equal(make_ascii(vec), as.character(1:3))
  expect_equal(make_ascii(paste0(vec, "a")), paste0(1:3, "a"))
  expect_equal(make_ascii(paste0(vec, "a"), numerize = TRUE),
               as.character(1:3))
})

test_that("headerize() change specific row into df header", {
  df <- data.frame(a = 1:10, b = 11:20, c = 21:30)
  expect_equal(colnames(headerize(df, 1)), c("1", "11", "21"))
  expect_equal(colnames(headerize(df, 2)), c("2", "12", "22"))
  expect_equal(colnames(headerize(df, 3)), c("3", "13", "23"))

  df      <- data.frame(a = 1:10, b = 11:20, c = 21:30)
  df[1, ] <- rep("foo", 3)
  df
  headerize(df, 1) %>% data.frame()
  expect_equal(colnames(headerize(df, 1)), c("foo", "foo.1", "foo.2"))
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
  data <- load_alldata("excels/sheetname.xlsx", sheet = "foo") %>%
    headerize(1) %>%
    structure(sheetname = "foo")
  conv <- sheet2var(data, as = "misc")
  expect_equal(unique(conv$misc), "foo")
  conv <- sheet2var(data, as = "bar")
  expect_equal(unique(conv$bar), "foo")
})

test_that("unfiscalize() converts fiscal year column of given df", {
  df_fiscal_jp <- data.frame(fisy = rep(2019, 12), month = c(4:12, 1:3))
  df_trueyear  <- unfiscalize(df_fiscal_jp, ycol = 1, mcol = 2,
                           month_start = 4, rule = "tail")
  expect_equal(df_trueyear$year, c(rep(2019, 9), rep(2020, 3)))

  df_fiscal_us <- data.frame(fisy = rep(2020, 12), month = c(10:12, 1:9))
  df_trueyear  <- unfiscalize(df_fiscal_us, ycol = 1, mcol = 2,
                           month_start = 10, rule = "head")
  expect_equal(df_trueyear$year, c(rep(2019, 3), rep(2020, 9)))

  df_fiscal_us <- data.frame(year = rep(2020, 12), month = c(10:12, 1:9))
  df_trueyear  <- unfiscalize(df_fiscal_us, ycol = 1, mcol = 2,
                           month_start = 10, rule = "head")
  expect_equal(df_trueyear$trueyr, c(rep(2019, 3), rep(2020, 9)))
})

test_that("crop() returns cropped bf before match", {
  data <- data.frame(a = letters, b = 1:26, stringsAsFactors = FALSE)
  expect_equal(crop(data, direction = "v", pos = 1, regex = "p"),
               data.frame(a = letters[1:16], b = 1:16,
                          stringsAsFactors = FALSE))

  data_horiz <- data.frame(t(data))
  expect_equal(crop(data_horiz, direction = "h", pos = 1, regex = "p"),
               data.frame(t(data.frame(a = letters[1:16],
                                       b = 1:16,
                                       stringsAsFactors = FALSE))))
})

test_that("crop() returns cropped df after match", {
  data <- data.frame(a = letters, b = 1:26, stringsAsFactors = FALSE)
  expect_equal(crop(data, direction = "v", pos = 1,
                    regex = "p", use_after = TRUE),
               data.frame(a = letters[16:26], b = 16:26,
                          row = 16:26, stringsAsFactors = FALSE,
                          row.names = "row"))

  data_horiz <- data.frame(t(data), stringsAsFactors = FALSE)
  expect_equal(crop(data_horiz, direction = "h", pos = 1,
                    regex = "x", use_after = TRUE),
               data.frame(row = c("a", "b"),
                          X24 = c("x", "24"),
                          X25 = c("y", "25"),
                          X26 = c("z", "26"),
                          row.names = "row", stringsAsFactors = FALSE))
})

test_that("crop() do nothing if direction == NULL", {
  data <- data.frame(a = letters, b = 1:26, stringsAsFactors = FALSE)
  expect_equal(crop(data, direction = NULL), data)
})

test_that("pull_vector() make vector from a row of df", {
  data <- data.frame(a = letters, b = 1:26, stringsAsFactors = FALSE)
  expect_equal(pull_vector(data, direction = "h", pos = 1),
               c("a", "1"))
  expect_equal(pull_vector(data, direction = "h", pos = 2),
               c("b", "2"))
  expect_equal(pull_vector(data, direction = "h", pos = 26),
               c("z", "26"))
})

test_that("pull_vector() make vector from a column of df", {
  data <- data.frame(a = letters, b = 1:26, stringsAsFactors = FALSE)
  expect_equal(pull_vector(data, direction = "v", pos = 1), letters)
  expect_equal(pull_vector(data, direction = "v", pos = 2), 1:26)
})
