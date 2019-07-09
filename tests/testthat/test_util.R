context("Test utils")

test_that("add_reference() add 'fname' and 'sheet' columns to df", {
  fname <- "YrowMcol.xlsx"
  sheet <- "Sheet1"
  data <- readxl::read_excel(fname, sheet = sheet) %>%
    add_reference(fname, sheet) %>%
    data.frame()
  expect_equal(colnames(data), c("year", paste0("X", 1:12), "fname", "sheet"))
})

test_that("ceasefire() returns massage and df", {
  df     <- data.frame(a = 1:5, b = 6:10)
  return <- ceasefire(df, path = "foo", sheet = "bar", funcname = "baz")
  expect_equal(return$a, as.character(1:5))
  expect_equal(return$fname, rep("foo", 5))
  expect_equal(return$sheet, rep("bar", 5))
})
