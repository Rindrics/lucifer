context("Rebel against godly Excel workbook")

test_that("rebel() beat up file with merged header", {
  fname <- "excels/merged.xlsx"
  beaten <- rebel(path = fname, sheet_regex = "Sheet.",
                  row_headers = 1:2, col_headers = 1:2,
                  cluster = list(regex = "NA_A2",
                                 dir = "h",
                                 pos = 1,
                                 offset = c(0, 0),
                                 ends = list(row = "A16",
                                             col = "E1_H2"))) %>%
    as.data.frame()
  expect_equal(dplyr::pull(beaten, 1),
               rep(c(rep("A2", 7), rep("A10", 6), rep("A16", 8)), 2))
  expect_equal(vectorize_row(beaten, 2),
              c("A2", paste0(LETTERS[c(1, 3:8)], 4), fname, "Sheet1"))
  expect_equal(vectorize_row(beaten, 23),
              c("A2", paste0(LETTERS[c(1, 3:8)], 14), fname, "Sheet2"))
})

test_that("rebel() throws an error", {
    expect_success(
      expect_warning(
        rebel(path = "excels/clustered.xlsx", sheet_regex = "[0-9]+",
              cluster = list(direction = "foo",
                             pos = 1, regex = "b..",
                             offset = c(1, 0),
                             ends = list(row = "A5", col = "test"))),
         "Set 'direction' correctly"))
})

test_that("rebel() beat up file with clustered data", {
  beaten <- rebel(path = "excels/clustered.xlsx", sheet_regex = "[0-9]+",
                  cluster = list(dir = "h",
                                 pos = 1,
                                 regex = "b..",
                                 offset = c(1, 0),
                                 ends = list(row = "A5", col = "test"))) %>%
    as.data.frame()
  expect_equal(colnames(beaten), c("this", "is", "a", "test", "fname", "sheet"))
  expect_equal(as.numeric(dplyr::pull(beaten, 2)),
               rep(c(12:15, 22:25, 32:35), 2))
  expect_equal(as.numeric(dplyr::pull(beaten, 3)),
               rep(c(12:15, 22:25, 32:35), 2) + 30)
  expect_equal(as.numeric(dplyr::pull(beaten, 4)),
               rep(c(12:15, 22:25, 32:35), 2) + 60)
})

test_that("rebel() beat up file with YrowMcol data", {
  beaten <- rebel(path = "excels/YrowMcol.xlsx", sheet_regex = "Sheet.",
                  cluster = list(dir = "v",
                                 regex = "year",
                                 pos = 1,
                                 offset = c(0, 0),
                                 ends = c(row = "2000",
                                          col = "12")),
                  row_type = "Y",
                  col_type = list(regex = "^1?[0-9]月?",
                                  newname = "month",
                                  varname = "given_varname"))
  expect_equal(colnames(beaten),
               c("year", "fname", "sheet", "month", "given_varname"))
  expect_equal(unique(beaten$year), 1969:2000)
  expect_equal(unique(beaten$month), 1:12)
  expect_setequal(unique(beaten$given_varname), as.character(c(1:384, 401:784)))
})

test_that("rebel() beat up file contaminated by summary row", {
  beaten <- rebel(path = "excels/sumrow_contami.xlsx", sheet_regex = "Sheet.",
                  cluster = list(regex = "A1$",
                                 dir = "h",
                                 pos = 1,
                                 offset = c(0, 0),
                                 ends = list(row = "A30",
                                             col = "H1")),
                  row_omit = list(key = "sum",
                                  colpos = 1,
                                  regex = FALSE))
  expect_equal(colnames(beaten), c(paste0(LETTERS[1:8], 1), "fname", "sheet"))
  expect_setequal(
    dplyr::pull(beaten, 2),
    as.character(c(2:10, 12:20, 22:30, 212:220, 222:230, 232:240)))

  beaten <- rebel(path = "excels/sumrow_contami.xlsx", sheet_regex = "Sheet.",
                  cluster = list(regex = "A1$",
                                 dir = "h",
                                 pos = 1,
                                 offset = c(0, 0),
                                 ends = list(row = "A30",
                                             col = "H1")),
                  row_omit = list(key = "s..",
                                  colpos = 1,
                                  regex = TRUE))
  expect_equal(colnames(beaten), c(paste0(LETTERS[1:8], 1), "fname", "sheet"))
  expect_setequal(
    dplyr::pull(beaten, 2),
    as.character(c(2:10, 12:20, 22:30, 212:220, 222:230, 232:240)))
})

test_that("rebel() beat up file contaminated by summary column", {
  beaten <- rebel(path = "excels/sumcol_contami.xlsx", sheet_regex = "Sheet.",
                  cluster = list(regex = "A1$",
                                 dir = "h",
                                 pos = 1,
                                 offset = c(0, 0),
                                 ends = list(row = "A30",
                                             col = "H1")),
                  col_omit = list(key = "sum",
                                  rowpos = 1,
                                  regex = FALSE))
  expect_equal(colnames(beaten),
               c(paste0(LETTERS[c(1:3, 5:6, 8)], 1), "fname", "sheet"))
  expect_setequal(dplyr::pull(beaten, 2), as.character(c(2:30, 212:240)))

  beaten <- rebel(path = "excels/sumcol_contami.xlsx", sheet_regex = "Sheet.",
                  cluster = list(dir = "v",
                                 regex = "A1$",
                                 pos = 1,
                                 offset = c(0, 0),
                                 ends = c(row = "A30",
                                          col = "H1")),
                  col_omit = list(key = "s..",
                                  rowpos = 1,
                                  regex = TRUE))
  expect_equal(colnames(beaten),
               c(paste0(LETTERS[c(1:3, 5:6, 8)], 1), "fname", "sheet"))
  expect_setequal(dplyr::pull(beaten, 2), as.character(c(2:30, 212:240)))
})

test_that("early return", {
  returned <- rebel(path = "excels/sumcol_contami.xlsx", sheet_regex = "Sheet.")
  expect_equal(colnames(returned)[1:3], paste0("(dir='v',pos=", 1:3, ")"))
  expect_equal(rownames(returned)[1:3], paste0("(dir='h',pos=", 1:3, ")"))
})
