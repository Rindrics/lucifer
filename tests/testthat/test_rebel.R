context("Rebel against godly Excel workbook")

test_that("rebel() beat up file with merged header", {
  fname <- "merged.xlsx"
  beaten <- rebel(path = fname, sheet_regex = "Sheet.",
                        row_merged = 1, col_merged = 1) %>%
    as.data.frame()
  beaten
  expect_equal(as.vector(beaten[, 1]),
               rep(c(rep("A2", 7), rep("A10", 6), rep("A16", 8)), 2))
  expect_equal(as.vector(unlist(beaten[1, ])),
              c("A2", paste0(LETTERS[c(1, 3:8)], 3), fname, "Sheet1"))
  expect_equal(as.vector(unlist(beaten[22, ])),
              c("A2", paste0(LETTERS[c(1, 3:8)], 13), fname, "Sheet2"))
})

test_that("rebel() beat up file with clustered data", {
  beaten <- rebel(path = "clustered.xlsx", sheet_regex = "[0-9]+",
                  cluster = list(dir = "col",
                                 pos = 1,
                                 regex = "b..",
                                 offset = c(1, 0),
                                 dim = c(5, 4))) %>%
    as.data.frame()
  beaten
  expect_equal(colnames(beaten), c("this", "is", "a", "test", "fname", "sheet"))
  expect_equal(as.numeric(dplyr::pull(beaten, 2)),
               rep(c(12:15, 22:25, 32:35), 2))
  expect_equal(as.numeric(dplyr::pull(beaten, 3)),
               rep(c(12:15, 22:25, 32:35), 2) + 30)
  expect_equal(as.numeric(dplyr::pull(beaten, 4)),
               rep(c(12:15, 22:25, 32:35), 2) + 60)
})

test_that("rebel() beat up file with YrowMcol data", {
  beaten <- rebel(path = "YrowMcol.xlsx", sheet_regex = "Sheet.",
                        row_type = "Y",
                        col_type = list(regex = "^1?[0-9]月?",
                                        newname = "month",
                                        varname = "given_varname"))
  expect_equal(colnames(beaten),
               c("year", "fname", "sheet", "month", "given_varname"))
  expect_equal(unique(beaten$year), as.character(1969:2000))
  expect_equal(unique(beaten$month), as.character(1:12))
  expect_setequal(unique(beaten$given_varname), as.character(c(1:384, 401:784)))
})

test_that("rebel() beat up file contaminated by summary row", {
  beaten <- rebel(path = "sumrow_contami.xlsx", sheet_regex = "Sheet.",
                  row_omit = list(key = "sum",
                                  colpos = 1,
                                  regex = FALSE))
  expect_equal(colnames(beaten), c(paste0(LETTERS[1:8], 1), "fname", "sheet"))
  beaten <- beaten %>%
    dplyr::mutate(B1 = as.numeric(B1),
                  C1 = as.numeric(C1)) %>%
    dplyr::arrange(B1)
  expect_equal(unique(beaten$B1),
               c(2:10, 12:20, 22:30, 212:220, 222:230, 232:240))

  beaten <- rebel(path = "sumrow_contami.xlsx", sheet_regex = "Sheet.",
                  row_omit = list(key = "s..",
                                  colpos = 1,
                                  regex = TRUE))
  beaten
  expect_equal(colnames(beaten), c(paste0(LETTERS[1:8], 1), "fname", "sheet"))
  beaten <- beaten %>%
    dplyr::mutate(B1 = as.numeric(B1),
                  C1 = as.numeric(C1)) %>%
    dplyr::arrange(B1)
  expect_equal(unique(beaten$B1),
               c(2:10, 12:20, 22:30, 212:220, 222:230, 232:240))
})

test_that("rebel() beat up file contaminated by summary column", {
  beaten <- rebel(path = "sumcol_contami.xlsx", sheet_regex = "Sheet.",
                  col_omit = list(key = "sum",
                                  rowpos = 1,
                                  regex = FALSE))
  beaten
  expect_equal(colnames(beaten),
               c(paste0(LETTERS[c(1:3, 5:6, 8)], 1), "fname", "sheet"))
  beaten <- beaten %>%
    dplyr::mutate(B1 = as.numeric(B1),
                  C1 = as.numeric(C1)) %>%
    dplyr::arrange(B1)
  expect_equal(unique(beaten$B1), c(2:30, 212:240))

  beaten <- rebel(path = "sumcol_contami.xlsx", sheet_regex = "Sheet.",
                  col_omit = list(key = "s..",
                                  rowpos = 1,
                                  regex = TRUE))
  beaten
  expect_equal(colnames(beaten),
               c(paste0(LETTERS[c(1:3, 5:6, 8)], 1), "fname", "sheet"))
  beaten <- beaten %>%
    dplyr::mutate(B1 = as.numeric(B1),
                  C1 = as.numeric(C1)) %>%
    dplyr::arrange(B1)
  expect_equal(unique(beaten$B1), c(2:30, 212:240))
})

test_that("rebel() beat up file contaminated by full-width characters", {
  beaten <- rebel(path = "fullwidth.xlsx", sheet_regex = "Sheet.",
                  fullwidth = list(colpos = 1,
                                   numerize = TRUE))
  expect_equal(dplyr::pull(beaten, 1), rep(1:5, 2))

  beaten <- rebel(path = "fullwidth.xlsx", sheet_regex = "Sheet.",
                  fullwidth = list(colpos = 1,
                                   numerize = FALSE))
  expect_equal(dplyr::pull(beaten, 1), rep(as.character(1:5), 2))

  beaten <- rebel(path = "fullwidth.xlsx", sheet_regex = "Sheet.",
                  fullwidth = list(colpos = 2,
                                   numerize = TRUE))
  expect_equal(dplyr::pull(beaten, 2), rep(11:15, 2))

  beaten <- rebel(path = "fullwidth.xlsx", sheet_regex = "Sheet.",
                  fullwidth = list(colpos = 6,
                                   numerize = TRUE))
  expect_equal(dplyr::pull(beaten, 6), rep(1:5, 2))

  beaten <- rebel(path = "fullwidth.xlsx", sheet_regex = "Sheet.",
                  fullwidth = list(colpos = 6,
                                   numerize = FALSE))
  expect_equal(dplyr::pull(beaten, 6), rep(paste0(1:5, "月"), 2))
})
