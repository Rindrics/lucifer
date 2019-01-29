context("Load .hdr file")

infile   <-
  "../Genus-spcs/survey/mtfoo/data/Sardinops-melanostictus_foo_MT01_01.hdr"
data     <- load_hdr(infile)
varnames <-  c("標本番号", "採集航海番号",  "採集ｽﾃｰｼｮﾝ番号", "採集日付",
               "緯度", "経度", "表面水温", "体長", "体重", "耳石径", "耳左右",
               "計測者番号", "日輪数", "lens", "calib", "unit",
               "samplesize", "filename", "日輪幅")

test_that("load_hdr() reads '.hdr' file correctly", {
  expect_is(data, "data.frame")
  expect_setequal(data$V1[1:19], varnames)
})

test_that("locate_1stinc() returns the position of first inc", {
  expect_equal(locate_1stinc(data$V1), 20)
})

test_that("get_incdata() returns incdata", {
  expect_equal(get_incdata(append(varnames, 1:10)), 1:10)
})
