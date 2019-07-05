context("Fight with merged cells")

test_that("nourin", {
  fname <- "nourin.xlsx"
  browser()
  data <- fname %>%
    rebel(sheet_regex = ".+",
                   cluster = list(regex = "合.+計",
                                  dir = "v",
                                  pos = 1,
                                  offset = c(-4, 0),
                                  ends = list(row = "その他",
                                              col = "海.+藻.+類")),
                   row_merged = 1) %>%
    as.data.frame()
  data[, 1]
  data[, 2]
  data[, 3]
  data[, 4]
  data[, 5]
  data[, 6]

  data[1, ]
  data[2, ]
  data[3, ]
  data[4, ]
  data[5, ]

  data <- fname %>%
    rebel(sheet_regex = ".+",
                   cluster = list(regex = "合.+計",
                                  dir = "v",
                                  pos = 1,
                                  offset = c(-4, 0),
                                  ends = list(row = "その他",
                                              col = "海.+藻.+類"))) %>%
    as.data.frame()
  data[, 1]
  data[, 2]
  data[, 3]
  data[, 4]
  data[, 5]
  data[, 6]

  data[1, ]
  data[2, ]
  data[3, ]
  data[4, ]
  data[5, ]
  head(data)
  expect_equal("a", "b")
})
