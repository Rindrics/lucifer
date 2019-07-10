context("Fight with bad row header")

fname <- "nourin.xlsx"

test_that("tidy_rowhead() works as a shapetool", {
  good_header <- readxl::read_excel(fname, sheet = "goodheader",
                                    range = "B10:F44", col_names = FALSE) %>%
    itemize()
  baddata  <- readxl::read_excel(fname, sheet = "北海道",
                                 range = "B10:K44", col_names = FALSE)
  shaped <- tidy_rowhead(baddata, range_header = 1:5, into = good_header)

  expect_equal(colnames(shaped),
               c(paste0("group", 4:1), "item", paste0("...", 6:10)))
  expect_equal(vectorize_row(shaped, 7),
               c("網漁業", "まき網", "大中型まき網", "１そう",
                 "遠洋かつお・まぐろまき網", "8", "-", "-", "-", "-"))
})

test_that("tidy_rowhead works in rebel()", {
  #browser()
  data <- fname %>%
    rebel(sheet_regex = ".+",
          row_merged = 1:5,
          col_merged = 6:9,
                   cluster = list(regex = "合.+計",
                                  dir = "v",
                                  pos = 1,
                                  offset = c(0, -1),
                                  ends = list(row = "その他",
                                              col = "海.+藻.+類")))
  data
  expect_equal(1, 2)
  data %>% tidy_rowhead(range_header = 1:5, into = good_header)
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
    rebel(sheet_regex = "^(?!.*matomo).*$",
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
