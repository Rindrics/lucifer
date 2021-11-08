context("Load data")

test_that("load_alldata loads Excel file as text", {
  data <- load_alldata(path = "excels/various_class.xlsx", sheet = "Sheet1")
  expect_is(data, "data.frame")
  expect_equal(class(dplyr::pull(data, 1)), "character")
  expect_equal(class(dplyr::pull(data, 2)), "character")
  expect_equal(class(dplyr::pull(data, 3)), "character")
  expect_equal(class(dplyr::pull(data, 4)), "character")
})

test_that("load_alldata loads aomori data as text", {
  data <- load_alldata(path = "excels/actual_cases/clustered_wide.xlsx", sheet = "マイワシ")
  expect_is(data, "data.frame")
  expect_equal(class(dplyr::pull(data, 1)), "character")
  expect_equal(class(dplyr::pull(data, 2)), "character")
  expect_equal(class(dplyr::pull(data, 3)), "character")
  expect_equal(class(dplyr::pull(data, 4)), "character")
})
