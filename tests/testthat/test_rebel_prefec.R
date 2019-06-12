context("Rebel prefectures using rebel()")

test_that("rebel_sheet() beat aomori data up", {
  fname <- "aomori.xlsx"
  sheet <- "マイワシ"
  beaten <- rebel_sheet(path = fname, sheet = sheet,
                        cluster = list(dir = "row",
                                       pos = 1,
                                       regex = "^年",
                                       offset = c(0, 0),
                                       dim = c(40, 19),
                                       info = list(offset = c(-2, 0),
                                                   dim = c(1, 2),
                                                   headerized = FALSE)),
                        row_type = "Y",
                        col_type = list(regex = "^1?[0-9]月?",
                                        newname = "month",
                                        varname = "catch"))
  expect_setequal(beaten$catch, as.character(1:936))
  expect_equal(unique(beaten$month), paste0(1:12, "月"))
  expect_equal(unique(beaten$`年／月`), as.character(1981:2019))
  expect_equal(unique(beaten$漁法), c("まき網漁業", "定置網漁業（底建網含む）"))
  expect_equal(unique(beaten$fname), fname)
  expect_equal(unique(beaten$sheet), sheet)
})
