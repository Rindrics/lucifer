context("Handle file name")
fn_kumamoto  <- "04 漁獲努力量（1704~1803）（熊本県）.xls"
fn_kagoshima <- "体長組成(H29年度).xlsx"
tbl_fname <- hash::hash(c(fn_kumamoto, fn_kagoshima), c("kumamoto", "kagoshima"))
test_that("give_class() gives fname its file format as class", {
  expect_is(give_class(fn_kumamoto, tbl_fname), "kumamoto")
  expect_is(give_class(fn_kagoshima, tbl_fname), "kagoshima")
})
