context("Test utils")

test_that("add_reference() add 'fname' and 'sheet' columns to df", {
  fname <- "YrowMcol.xlsx"
  sheet <- "Sheet1"
  data <- readxl::read_excel(fname, sheet = sheet) %>%
    add_reference(fname, sheet) %>%
    data.frame()
  expect_equal(colnames(data), c("year", paste0("X", 1:12), "fname", "sheet"))
})
