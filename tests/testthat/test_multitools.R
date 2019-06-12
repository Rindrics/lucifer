context("Fights using multitools")

test_that("Fight with aomori using multitools", {
  df <- load_alldata("aomori.xlsx", sheet = "マイワシ")
  converted <- df %>%
    extract_clusters(regex = "^年", col = 1,
                     offset = c(0, 0), dim = c(40, 19),
                     info = list(offset = c(-2, 0),
                                 dim = c(1, 2))) %>%
    lapply(headerize, row = 1) %>%
    purrr::invoke(rbind, .) %>%
    rm_nacols() %>%
    gather_cols(regex = "(1|2)?[0-9]月",
                newname = "month", varname = "catch")
  expect_equal(colnames(converted), c("年／月", "漁法", "month", "catch"))
  expect_equal(unique(converted$漁法), c("まき網漁業", "定置網漁業（底建網含む）"))
  expect_equal(unique(converted$month), paste0(1:12, "月"))
  expect_setequal(converted$catch, as.character(seq(1, 39 * 24)))
})
