context("Handle file name")
fname     <- "04 漁獲努力量（1704~1803）（熊本県）.xls"
tbl_fname <- hash::hash(c("04 漁獲努力量（1704~1803）（熊本県）.xls"), "熊本")
tbl_fmt   <- hash::hash(c("熊本"), c("seimitsu"))
test_that("give_class() gives fname its file format as class", {
  expect_is(give_class(fname, tbl_fname, tbl_fmt), "seimitsu")
})
