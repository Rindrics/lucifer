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
  expect_message(
    return <- ceasefire(df, path = "foo", sheet = "bar", funcname = "baz"),
    "Good. Specify 'baz' next."
  )
  expect_equal(colnames(return),
               c(paste0("(dir='v',pos=", 1:2, ")"), "fname", "sheet"))
  expect_equal(rownames(return),
               paste0("(dir='h',pos=", 1:5, ")"))
  expect_equal(return[, 1], 1:5)
  expect_equal(return[, 3], rep("foo", 5))
  expect_equal(return[, 4], rep("bar", 5))

  return <- ceasefire(df, path = "foo", sheet = "bar",
                      funcname = "baz", posnames = FALSE)
  expect_equal(colnames(return),
               c(letters[1:2], "fname", "sheet"))
  expect_equal(return[, 1], 1:5)
  expect_equal(return[, 3], rep("foo", 5))
  expect_equal(return[, 4], rep("bar", 5))
})
