context("rebel data with zenkaku column")

test_that("miyagi", {
  year <- 2019
  miyagi <- rebel(path = "excels/miyagi.xlsx", sheet_regex = "Sheet1",
        cluster = list(dir = "h", pos = 1 , regex = ".+", offset = c(2,0),
                       ends = list(row = paste0(year,"年"), col = "１２月"),
                       info = list(value_offset = c(-1,0),
                                   value_dim = c(1,1))),
        row_type = "Y",
        col_type=list(regex="^.+月",
                      newname = "month",
                      varname = "catch_ton"))

  expect_equal(unique(miyagi$year), 1995:2019)
  expect_equal(unique(miyagi$month), 1:12)
  expect_setequal(unique(miyagi$catch_ton),
                  as.character(c(1:300, 1201:1500, 2401:2700, 3601:3900)))
})
