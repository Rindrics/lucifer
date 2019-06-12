context("Rebel against godly Excel sheet")

test_that("rebel_sheet() beat up file with merged header", {
  fname <- "merged.xlsx"
  sheet <- "Sheet1"
  beaten <- rebel_sheet(path = fname, sheet = sheet,
                        row_merged = 1, col_merged = 1) %>%
    as.data.frame()
  beaten
  expect_equal(as.vector(beaten[, 1]),
               c(rep("A2", 7), rep("A10", 6), rep("A16", 8)))
  expect_equal(as.vector(unlist(beaten[1, ])),
               c("A2", paste0(LETTERS[c(1, 3:8)], 3), fname, sheet))
})

test_that("rebel_sheet() beat up file with clustered data", {
  beaten <- rebel_sheet(path = "clustered.xlsx", sheet = "foo",
                        cluster = list(dir = "col",
                                       pos = 1,
                                       regex = "...",
                                       offset = c(1, 0),
                                       dim = c(3, 4)))
  expect_equal(colnames(beaten), c("this", "is", "a", "test", "fname", "sheet"))
  expect_equal(dplyr::pull(beaten, 1),
               rep(c("A2", "A3"), 4))
  expect_equal(dplyr::pull(beaten, 2),
               as.character(c(2, 3, 12, 13, 22, 23, 32, 33)))
  expect_equal(dplyr::pull(beaten, 4),
               as.character(c(62, 63, 72, 73, 82, 83, 92, 93)))

  beaten <- rebel_sheet(path = "clustered.xlsx", sheet = "foo",
                  cluster = list(dir = "col",
                                 pos = 1,
                                 regex = "b..",
                                 offset = c(1, 2),
                                 dim = c(5, 2)))
  expect_equal(colnames(beaten), c("a", "test", "fname", "sheet"))
  expect_equal(as.numeric(dplyr::pull(beaten, 1)), c(42:45, 52:55, 62:65))
  expect_equal(as.numeric(dplyr::pull(beaten, 2)), c(72:75, 82:85, 92:95))
})

test_that("rebel_sheet() beat up file with YrowMcol data", {
  beaten <- rebel_sheet(path = "YrowMcol.xlsx", sheet = "Sheet1",
                        row_type = "Y",
                        col_type = list(regex = "^1?[0-9]月?",
                                        newname = "month",
                                        varname = "given_varname"))
  expect_equal(colnames(beaten),
               c("year", "fname", "sheet", "month", "given_varname"))
  expect_equal(unique(beaten$year), as.character(1969:2000))
  expect_equal(unique(beaten$month), as.character(1:12))
  expect_setequal(unique(beaten$given_varname), as.character(1:384))
})

test_that("rebel_sheet() beat up file contaminated by summary column", {
  beaten <- rebel_sheet(path = "sumcol_contami.xlsx", sheet = "Sheet1",
                        col_omit = list(key = "sum",
                                        rowpos = 1,
                                        regex = FALSE))
  beaten
  expect_equal(colnames(beaten),
               c(paste0(LETTERS[c(1:3, 5:6, 8)], 1), "fname", "sheet"))
  beaten <- beaten %>%
    dplyr::mutate(B1 = as.numeric(B1),
                  C1 = as.numeric(C1))
  expect_equal(unique(beaten$B1), 2:30)
  expect_equal(unique(beaten$C1), 32:60)
})

test_that("rebel_sheet() beat up file contaminated by summary row", {
  beaten <- rebel_sheet(path = "sumrow_contami.xlsx", sheet = "Sheet1",
                        row_omit = list(key = "sum",
                                        colpos = 1,
                                        regex = FALSE))
  beaten
  expect_equal(colnames(beaten),
               c(paste0(LETTERS[c(1:8)], 1), "fname", "sheet"))
  beaten <- beaten %>%
    dplyr::mutate(B1 = as.numeric(B1),
                  C1 = as.numeric(C1))
  expect_equal(unique(beaten$B1), c(2:10, 12:20, 22:30))
  expect_equal(unique(beaten$C1), c(32:40, 42:50, 52:60))
})

test_that("rebel_sheet() beat up file contaminated by full-width characters", {
  beaten <- rebel_sheet(path = "fullwidth.xlsx", sheet = "Sheet1",
                        fullwidth = list(colpos = 1,
                                         numerize = TRUE))
  expect_equal(dplyr::pull(beaten, 1), 1:5)

  beaten <- rebel_sheet(path = "fullwidth.xlsx", sheet = "Sheet1",
                        fullwidth = list(colpos = 1,
                                         numerize = FALSE))
  expect_equal(dplyr::pull(beaten, 1), as.character(1:5))
  beaten <- rebel_sheet(path = "fullwidth.xlsx", sheet = "Sheet1",
                        fullwidth = list(colpos = 2,
                                         numerize = TRUE))
  expect_equal(dplyr::pull(beaten, 2), 11:15)

  beaten <- rebel_sheet(path = "fullwidth.xlsx", sheet = "Sheet1",
                        fullwidth = list(colpos = 6,
                                         numerize = TRUE))
  expect_equal(dplyr::pull(beaten, 6), 1:5)

  beaten <- rebel_sheet(path = "fullwidth.xlsx", sheet = "Sheet1",
                        fullwidth = list(colpos = 6,
                                         numerize = FALSE))
  expect_equal(dplyr::pull(beaten, 6), paste0(1:5, "月"))
})
