context("Columns with 'NA' colnames")

test_that("", {
  fname = "excels/nacol.xlsx"
  year <- 2019
  parsed_years <- lucifer::rebel(path = fname,
                   sheet_regex = "ﾏｲﾜｼ表", # str_extract
                   row_headers = NULL,
                   col_headers = NULL,
                   cluster = list(
                     dir = "v",
                     pos = 2,
                     regex = ".*表.*", # str_which
                     offset = c(1, 0),
                     ends = list(row = as.character(
                       difftime(as.Date(paste0(year, "-01-01")), 
                                as.Date("1899-12-30"))), # str_which
                       col = "^12$"),
                     info = list(value_offset = c(-1, 0), 
                                 value_dim = c(1, 0))
                   ),
                   row_type = "Y",
                   col_type = list(regex = "[1]?[0-9]",
                                   newname = "month",
                                   varname = "catch_ton"),
                   row_omit = NULL,
                   col_omit = NULL,
                   unfiscalize = c(month_start = NULL, rule = NULL)) %>%
    dplyr::filter(!is.na(topleft)) %>%
    dplyr::pull(topleft) %>%
    unique()
    
  expect_equal(parsed_years, 
               c("33239", "33604", "33970", "34335", "34700", "35065", "35431",
                 "35796", "36161", "36526", "36892", "37257", "37622", "37987",
                 "38353", "38718", "39083", "39448", "39814", "40179", "40544",
                 "40909", "41275", "41640", "42005", "42370", "42736", "43101",
                 "43466", "32874"))
})
