context("cleanse row|col headers")

test_that("fill_colhead() fill NAs of merged columns", {
  merged  <- load_alldata("merged.xlsx", sheet = "Sheet1")
  newrow <- fill_colhead(merged, 1) %>%
    vectorize_row(1)
  expect_equal(newrow, c(NA, rep("A1", 3), rep("E1", 4)))
})

test_that("fill_colhead() fill NAs of multiple merged columns", {
  merged  <- load_alldata("multi_merged.xlsx", sheet = "Sheet1")
  row1 <- fill_colhead(merged, 1:3) %>%
    vectorize_row(1)
  expect_equal(row1, c(NA, rep("A1", 3), rep("E1", 4)))

  row2 <- fill_colhead(merged, 1:3) %>%
    vectorize_row(2)
  expect_equal(row2, c(rep("A2", 3), "D2", rep("E2", 2), rep("G2", 2)))

  row3 <- fill_colhead(merged, 1:3) %>%
    vectorize_row(3)
  expect_equal(row3, c(NA, "A3", rep("C3", 2), rep("E3", 2), rep("G3", 2)))
})

test_that("fill_rowhead() fill NAs of merged rows", {
  merged <- load_alldata("merged.xlsx", sheet = "Sheet1")
  newcol <- fill_rowhead(merged, cols = 1) %>%
    dplyr::pull(1) %>%
    as.vector()
  expect_equal(newcol, c(NA, rep("A2", 8), rep("A10", 6), rep("A16", 8)))
})

test_that("fill_rowhead() fill NAs of multiple merged rows", {
  merged <- load_alldata("multi_merged.xlsx", sheet = "Sheet1")

  col1 <- fill_rowhead(merged, cols = 1:3) %>%
    dplyr::pull(1) %>%
    as.vector()
  expect_equal(col1, c(NA, rep("A2", 8), rep("A10", 6), rep("A16", 8)))

  col2 <- fill_rowhead(merged, cols = 1:3) %>%
    dplyr::pull(2) %>%
    as.vector()
  expect_equal(col2, c("A1", "A2", rep("A3", 3), rep("A6", 4), rep("A10", 3),
                       rep("A13", 3), rep("A16", 4), rep("A20", 4)))

  col3 <- fill_rowhead(merged, cols = 1:3) %>%
    dplyr::pull(3) %>%
    as.vector()
  expect_equal(col3,
               c(rep(NA, 2), "C3", rep("C4", 2), rep("C6", 2),
                 rep("C8", 2), rep("C10", 2), "C12", rep("C13", 2), "C15",
                 rep("C16", 2), rep("C18", 2), rep("C20", 2), rep("C22", 2)))
})

test_that("handle data with multiple merged col and row headers", {
  merged <- load_alldata("multi_merged.xlsx", sheet = "Sheet1")

  foo <- merged %>%
    fill_rowhead(cols = 1:3) %>%
    fill_colhead(rows = 1:3)
  bar <- merged %>%
    fill_colhead(rows = 1:3) %>%
    fill_rowhead(cols = 1:3)
  expect_equal(foo, bar)
})
