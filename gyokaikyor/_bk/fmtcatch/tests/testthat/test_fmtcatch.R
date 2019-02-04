# library(fmtcatch)
# context('treat indir')
# indir             <- "../../../../DATA/2017（平成29）年度"
# indir_incorrect   <- "../../../../DATA/2017（平成29）年度/18鹿児島"
# INDIR             <- "../../../../DATA/2017（平成29）年度/18鹿児島"
# outdir            <- "../../../../output"
# errormsg          <- "Give me the correct filepath to the year directory.\nThe name format needed is 'path-to-directory/YYYY（年号YY）年度'."
#
# test_that("get fiscal year from indir", {
#   expect_identical(getFiscal(indir), 2017)
#   expect_error(getFiscal(indir_incorrect), errormsg)
#   expect_error(getFiscal("./"), errormsg)
# })
