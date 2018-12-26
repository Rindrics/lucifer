library(tidyverse)
context("Handle param object")
test_that("make_params accept only 'sengyo' or 'cruise'", {
  expect_error(make_params(indir = "./", "a"), "Tell me the correct type of data. Is it 'sengyo', or 'cruise?'", fixed = TRUE)
  expect_error(make_params(indir = "./", 1), "Tell me the correct type of data. Is it 'sengyo', or 'cruise?'", fixed = TRUE)
})
