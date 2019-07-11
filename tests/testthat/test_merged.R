context("Fight with bad row header")

fname <- "nourin.xlsx"

ideal_header_r <- readxl::read_excel(fname, sheet = "ideal",
                                    range = "B10:F44", col_names = FALSE) %>%
  itemize(header_of = "row")
ideal_header_c <- readxl::read_excel(fname, sheet = "ideal",
                                    range = "H7:CM9", col_names = FALSE) %>%
  itemize(header_of = "col")

test_that("tidy_rowhead() works as a shapetool", {
  baddata  <- readxl::read_excel(fname, sheet = "北海道",
                                 range = "B10:K44", col_names = FALSE)
  shaped <- tidy_header(baddata, type = "row",
                        range = 1:5, ideal = ideal_header_r)

  expect_equal(colnames(shaped),
               c(paste0("group", 4:1), "item", paste0("...", 6:10)))
  expect_equal(vectorize_row(shaped, 8),
               c("１そう", "大中型まき網", "まき網", "網漁業",
                 "遠洋かつお・まぐろまき網", "8", "1", "1", "1", "1"))
})

test_that("tidy_rowhead works in rebel()", {
  browser()
  data <- fname %>%
    rebel(sheet_regex = "北海道",
          row_header = 1:5,
          col_header = 7:9, ideal = list(row = ideal_header_r),
          cluster = list(dir = "v", pos = 7))
  data

  data <- fname %>%
    rebel(sheet_regex = "北海道",
          row_header = 1:5,
          col_header = 7:9, ideal = list(col = ideal_header_c))
})
