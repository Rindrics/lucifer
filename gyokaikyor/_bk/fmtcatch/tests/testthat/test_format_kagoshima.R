# library(fmtcatch)
# context('format data of Kagoshima')
# indir <- '../../../../DATA/2017（平成29）年度'
# errormsg1  <- "File missing!"
# errormsg2  <- "Give me the correct filepath to the year directory.\nThe name format needed is 'path-to-directory/YYYY（年号YY）年度"
#
# test_that("check kagoshima", {
#   expect_identical(format_kagoshima(indir, 1), "４港月計表（28年度　鹿児島）.xlsx")
#   expect_error(format_kagoshima("test", 1), errormsg2)
# })
