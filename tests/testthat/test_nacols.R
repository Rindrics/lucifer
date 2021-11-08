context("Columns with 'NA' colnames")

test_that("", {
  fname = "excels/nacol.xlsx"
  year <- 2020
  parsed_years <- lucifer::rebel(path = fname,
                   sheet_regex = "ﾏｲﾜｼ表", # str_extract
                   row_headers = NULL,
                   col_headers = NULL,
                   cluster = list(
                     dir = "v",
                     pos = 2,
                     regex = ".*表.*", # str_which
                     offset = c(1, 0),
                     ends = list(
                       row = ".+",
                       col = "^12"),
                     info = list(value_offset = c(-1, 0), 
                                 value_dim = c(1, 0))
                   ),
                   row_type = "Y",
                   col_type = list(regex = ".*[1]?[0-9]",
                                   newname = "month",
                                   varname = "catch_ton")) %>%
    dplyr::filter(!is.na(topleft)) %>%
    dplyr::pull(topleft) %>%
    unique()
  expect_equal(parsed_years, 
               c("33239.0", "33604.0", "33970.0", "34335.0", "34700.0", "35065.0", "35431.0",
                 "35796.0", "36161.0", "36526.0", "36892.0", "37257.0", "37622.0", "37987.0",
                 "38353.0", "38718.0", "39083.0", "39448.0", "39814.0", "40179.0", "40544.0",
                 "40909.0", "41275.0", "41640.0", "42005.0", "42370.0", "42736.0", "43101.0",
                 "43466.0", "43831.0", "32874.0"))
})

