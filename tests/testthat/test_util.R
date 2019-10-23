context("Returned df structure")

test_that("colname", {
  test_reference_output <- function(path, sheet, expect) {
    eval(bquote(expect_equal(
      readxl::read_excel(path = path, sheet = sheet) %>%
      add_reference(path, sheet) %>%
      data.frame() %>%
      add_reference(path = path, sheet = sheet) %>%
     colnames(),
      expect)))
  }

  test_reference_output(path = "excels/YrowMcol.xlsx", sheet = "Sheet1",
                        expect = c("year", paste0("X", 1:12), "fname", "sheet"))
})


test_that("ceasefire() returns message", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_message(
    return <- ceasefire(df, path = "foo", sheet = "bar", funcname = "baz"),
    "Good. Specify 'baz' next."
  )
})

test_that("format of the early returned df", {
  expect_equal_col <- function(path, sheet, col, expect) {
    eval(bquote(expect_equal(
      ceasefire(df, path = path,
                sheet = sheet, funcname = "baz") %>%
      dplyr::pull(col),
      expect)))
  }
  expect_equal_row <- function(path, sheet, row, expect) {
    eval(bquote(expect_equal(
      ceasefire(df, path = path,
                sheet = sheet, funcname = "baz") %>%
      vectorize_row(row),
      expect)))
  }
  expect_equal_col_name <- function(path, sheet, print, expect) {
    eval(bquote(expect_equal(
      ceasefire(df, path = path,
                sheet = sheet, funcname = "foo", print_posnames = print) %>%
      colnames(),
      expect)))
  }
  expect_equal_row_name <- function(path, sheet, print, expect) {
    eval(bquote(expect_equal(
      ceasefire(df, path = path,
                sheet = sheet, funcname = "foo", print_posnames = print) %>%
      rownames(),
      expect)))
  }


  df <- data.frame(a = 1:5, b = 6:10)
  expect_equal_col(path = "foo", sheet = "bar", col = 1,
                   expect = 1:5)
  expect_equal_col(path = "foo", sheet = "bar", col = 3,
                   expect = rep("foo", 5))
  expect_equal_col(path = "foo", sheet = "bar", col = 4,
                   expect = rep("bar", 5))
  expect_equal_col(path = "foo", sheet = "bar", col = 1,
                   expect = 1:5)
  expect_equal_col(path = "foo", sheet = "bar", col = 3,
                   expect = rep("foo", 5))
  expect_equal_col(path = "foo", sheet = "bar", col = 4,
                   expect = rep("bar", 5))

  expect_equal_row(path = "foo", sheet = "bar", row = 1,
                   c(1, 6, "foo", "bar"))
  expect_equal_row(path = "foo", sheet = "bar", row = 2,
                   c(2, 7, "foo", "bar"))
  expect_equal_row(path = "foo", sheet = "bar", row = 3,
                   c(3, 8, "foo", "bar"))
  expect_equal_col_name("foo", "bar", TRUE,
                        c(paste0("(dir='v',pos=", 1:2, ")"), "fname", "sheet"))
  expect_equal_row_name("foo", "bar", TRUE,
                        c(paste0("(dir='h',pos=", 1:5, ")")))
  expect_equal_col_name("foo", "bar", FALSE,
                        c("a", "b", "fname", "sheet"))
  expect_equal_row_name("foo", "bar", FALSE,
                        as.character(1:5))

  df <- data.frame(a = 1:10, b = 11:20, c = 21:30)
  expect_equal_col_name("foo", "bar", TRUE,
                        c(paste0("(dir='v',pos=", 1:3, ")"), "fname", "sheet"))
  expect_equal_row_name("foo", "bar", TRUE,
                        c(paste0("(dir='h',pos=", 1:10, ")")))
  expect_equal_col_name("foo", "bar", FALSE,
                        c("a", "b", "c", "fname", "sheet"))
  expect_equal_row_name("foo", "bar", FALSE,
                        as.character(1:10))
})
