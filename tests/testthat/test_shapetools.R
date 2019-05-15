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

test_that("rm_sumrow() remove summary rows from df", {
  contami <- load_alldata("colsum_contami.xlsx", sheet = "Sheet1")
  expect_equal(rm_sumrow(contami, key = "sum", colname = "A1") %>%
                 dplyr::pull(1),
               c(paste0("A", c(1:10, 12:20, 22:30))))
})

test_that("rm_sumcol() remove summary cols from df", {
  contami <- load_alldata("rowsum_contami.xlsx", sheet = "Sheet1")
  expect_equal(rm_sumcol(contami, key = "sum", rowname = "A1") %>%
                 dplyr::slice(1) %>%
                 unlist() %>%
                 as.vector(),
               c(paste0(LETTERS[1:8][c(-4, -7)], 1)))

  expect_equal(rm_sumcol(contami, key = "sum",
                         rowname = "A1$", regex = TRUE) %>%
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
