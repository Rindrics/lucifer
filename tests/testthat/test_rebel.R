context("Rebel against godly Excel")

test_that("rebel_sheet() beat up file with merged header", {
  beaten <- rebel_sheet(path = "merged.xlsx", sheet = "Sheet1",
                        row_merged = 1, col_merged = 1) %>%
    as.data.frame()
  expect_equal(as.vector(beaten[, 1]),
               c(rep("A2", 7), rep("A10", 6), rep("A16", 8)))
  expect_equal(as.vector(unlist(beaten[1, ])),
               c("A2", paste0(LETTERS[c(1, 3:8)], 3)))
})

test_that("rebel() beat up file with merged header", {
  beaten <- rebel(path = "merged.xlsx", sheet.regex = "Sheet.",
                        row_merged = 1, col_merged = 1) %>%
    as.data.frame()
  beaten
  expect_equal(as.vector(beaten[, 1]),
               rep(c(rep("A2", 7), rep("A10", 6), rep("A16", 8)), 2))
  expect_equal(as.vector(unlist(beaten[1, ])),
              c("A2", paste0(LETTERS[c(1, 3:8)], 3)))
})
