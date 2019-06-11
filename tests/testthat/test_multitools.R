test_that("Fight with aomori using multitools", {
  df <- load_alldata("aomori.xlsx", sheet = "マイワシ")
  converted <- df %>%
    extract_clusters(regex = "^年", col = 1,
                     offset = c(0, 0), dim = c(40, 19)) %>%
    lapply(headerize, row = 1) %>%
    purrr::invoke(rbind, .) %>%
    rm_nacols() %>%
    gather_cols(regex = "(1|2)?[0-9]月",
                newname = "foo", varname = "bar")
  expect_equal(colnames(converted), c("年／月", "foo", "bar"))
  expect_equal(unique(converted$foo), paste0(1:12, "月"))
  expect_equal(converted$bar, as.character(rep(1:39, 24)))
})
