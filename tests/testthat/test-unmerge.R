context("Test unmerge tools")

test_that("unmerge_horiz() fill NAs of merged columns", {
  with_na  <- load_alldata("merged.xlsx", sheet = "Sheet1")

  filled <- fill_na(with_na, along = "h", pos = 1)
  expect_equal(vectorize_row(filled, 1), c(NA, rep("A1", 3), rep("E1", 4)))

  filled <- fill_na(with_na, along = "v", pos = 1)
  expect_equal(dplyr::pull(filled, 1) %>%
               as.vector(),
               c(NA, rep("A2", 8), rep("A10", 6), rep("A16", 8)))
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
