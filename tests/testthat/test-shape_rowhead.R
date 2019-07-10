context("Test shapetools for merged headers")

test_that("itemize", {
  browser()
  expect_equal(2, 31)
  fname <- "nourin.xlsx"
  header <- readxl::read_excel(fname, sheet = "goodheader",
                               range = "B10:F44", col_names = FALSE) %>%
    as.matrix()
  header
})
