library(tidyverse)
context("Formatting loaded data")
param <- list()
param$indir <- "/Users/ahayashi/Documents/GitHub/tidyNAS/data/鮮魚関係"
param$type  <- "sengyo"
param$spcs  <- "カタクチイワシ"
# get_filelist(param)
infile <- "/Users/ahayashi/Documents/GitHub/tidyNAS/data/鮮魚関係/鮮魚測定06/カタクチイワシ.xls"
# get_sheet2read(infile)
sheet  <- "0125"
data   <- format(infile, sheet)
test_that("format() cleanses data", {
                                        # expect_match(object, regexp, ignore.case = TRUE)
                                        # expect_output(object, regexp, fixed = TRUE)
                                        # expect_error()
                                        # expect_message()
                                        # expect_is(model, "lm")
})
