context("Test shapetools for merged headers")

fname <- "nourin.xlsx"

test_that("itemize works for row header", {
  header <- readxl::read_excel(fname, sheet = "goodheader",
                               range = "B10:F44", col_names = FALSE) %>%
    as.matrix()

  shaped <- itemize(header, header_of = "row")
  expect_equal(nrow(shaped), 35)
  expect_equal(unique(shaped$group1), c(NA,
                                        "網漁業", "釣漁業", "捕鯨業", "その他"))
  expect_equal(unique(shaped$group2), c(NA, "底びき網", "まき網", "刺網",
                                        "定置網", "はえ縄", "はえ縄以外の釣"))
  expect_equal(unique(shaped$group4), c(NA, "１そう"))
})

test_that("itemize works for col header", {
  header <- readxl::read_excel(fname, sheet = "goodheader",
                               range = "H6:CM8", col_names = FALSE) %>%
    as.matrix()
  shaped <- itemize(header, header_of = "col")
  expect_equal(nrow(shaped), 84)
  expect_equal(
    unique(shaped$group1),
    c("漁獲量", "魚類", "まぐろ類", "かじき類", "かつお類", NA, "さけ・ます類",
      "", "いわし類", "あじ類", "ひらめ・かれい類", "たら類", "たい類",
      "えび類", "かに類", "貝類", "いか類", "海藻類")
  )
})
