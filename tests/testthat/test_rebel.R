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

test_that("rebel_sheet() beat up file with clustered data", {
  beaten <- rebel_sheet(path = "clustered.xlsx", sheet = "foo",
                        cluster = list(dir = "col",
                                       pos = 1,
                                       regex = "...",
                                       offset = c(1, 0),
                                       dim = c(3, 4)))
  expect_equal(colnames(beaten), c("this", "is", "a", "test"))
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
  expect_equal(colnames(beaten), c("a", "test"))
  expect_equal(as.numeric(dplyr::pull(beaten, 1)), c(42:45, 52:55, 62:65))
  expect_equal(as.numeric(dplyr::pull(beaten, 2)), c(72:75, 82:85, 92:95))
})

test_that("rebel() beat up file with merged header", {
  beaten <- rebel(path = "merged.xlsx", sheet_regex = "Sheet.",
                        row_merged = 1, col_merged = 1) %>%
    as.data.frame()
  expect_equal(as.vector(beaten[, 1]),
               rep(c(rep("A2", 7), rep("A10", 6), rep("A16", 8)), 2))
  expect_equal(as.vector(unlist(beaten[1, ])),
              c("A2", paste0(LETTERS[c(1, 3:8)], 3)))
})

test_that("rebel() beat up file with clustered data", {
  beaten <- rebel(path = "clustered.xlsx", sheet_regex = "[0-9]+",
                  cluster = list(dir = "col",
                                 pos = 1,
                                 regex = "b..",
                                 offset = c(1, 0),
                                 dim = c(5, 4))) %>%
    as.data.frame()
  expect_equal(colnames(beaten), c("this", "is", "a", "test"))
  expect_equal(as.numeric(dplyr::pull(beaten, 2)),
               rep(c(12:15, 22:25, 32:35), 2))
  expect_equal(as.numeric(dplyr::pull(beaten, 3)),
               rep(c(12:15, 22:25, 32:35), 2) + 30)
  expect_equal(as.numeric(dplyr::pull(beaten, 4)),
               rep(c(12:15, 22:25, 32:35), 2) + 60)
})
