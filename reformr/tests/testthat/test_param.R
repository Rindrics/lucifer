# library(tidyverse)
#   context("handle param object")
#   test_that("make_params accept only 'sengyo' or 'cruise'", {
#     expect_error(make_params(indir = "./", "a")
#                , "tell me the correct type of data. is it 'sengyo', or 'cruise?'", fixed = true)
#     expect_error(make_params(indir = "./", 1),
#                  "tell me the correct type of data. is it 'sengyo', or 'cruise?'", fixed = true)
#   })
#   test_that("make_params accept only 'カタクチイワシ', 'マイワシ' or 'マアジ' ", {
#     expect_error(make_params(indir = "./", "sengyo", 1), "tell me the correct japanese species name.", fixed = true)
#     expect_error(make_params(indir = "./", "sengyo", "anchovy"),  "tell me the correct japanese species name.", fixed = true)
#   })
