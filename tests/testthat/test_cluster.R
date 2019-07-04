context("Test extracting clustered data")

test_that("clusters distributed in row direction can be extracted", {
  data  <- data.frame(a = rep(c("foo", "bar", "baz", "bum"), 4),
                      b = 1:16, c = 11:26, d = 21:36, stringsAsFactors = FALSE)
  data2 <- extract_a_cluster(pos.key = 1, find_from = 1, direction = "row",
                             df = data, offset = c(0, 0),
                             ends = list(row = "bar", col = "21"))
  expect_equal(vectorize_row(data2, 1), c("foo", 1, 11, 21))
  expect_equal(data2$a, c("foo", "bar"))
  expect_equal(data2$b, 1:2)

  data2 <- extract_a_cluster(pos.key = 1, find_from = 1, direction = "row",
                             df = data, offset = c(1, 1),
                             ends = list(row = "3", col = "22"))
  expect_equal(data2$b, 2:3)
  expect_equal(data2$c, 12:13)

  data2 <- extract_a_cluster(pos.key = 1, find_from = 1, direction = "row",
                             df = data, offset = c(1, 1),
                             ends = list(row = "4", col = "22"))
  expect_equal(data2$b, 2:4)
  expect_equal(data2$c, 12:14)
  expect_equal(data2$d, 22:24)

  data2 <- extract_a_cluster(pos.key = 1, find_from = 1, direction = "row",
                             df = data, offset = c(2, 1),
                             ends = list(row = "4", col = "23"))
  expect_equal(data2$b, 3:4)
  expect_equal(data2$c, 13:14)
  expect_equal(data2$d, 23:24)

  data  <- data.frame(rbind(1:10, 11:20, 21:30, 31:40))
  data2 <- extract_a_cluster(pos.key = 1, find_from = 1, direction = "col",
                             df = data, offset = c(0, 0),
                             ends = list(row = "11", col = "2"))
  expect_equal(data2$X1, c(1, 11))
  expect_equal(data2$X2, c(2, 12))

  data2 <- extract_a_cluster(pos.key = 1, find_from = 1, direction = "col",
                             df = data, offset = c(1, 1),
                             ends = list(row = "32", col = "14"))
  expect_equal(data2$X2, c(12, 22, 32))
  expect_equal(data2$X3, c(13, 23, 33))
  expect_equal(data2$X4, c(14, 24, 34))

  data2 <- extract_a_cluster(pos.key = 1, find_from = 1, direction = "col",
                             df = data, offset = c(2, 1),
                             ends = list(row = "32", col = "24"))
  expect_equal(data2$X2, c(22, 32))
  expect_equal(data2$X3, c(23, 33))
  expect_equal(data2$X4, c(24, 34))
})

test_that("additional info can be appended to df", {
  data  <- load_alldata("cluster_info.xlsx", sheet = "Sheet1")
  data2 <- extract_a_cluster(pos.key = 5, find_from = 1, direction = "row",
                             df = data, offset = c(0, 0),
                             ends = list(row = "5", col = "bar"),
                             info = list(offset = c(-4, 0),
                                         dim = c(4, 2)))
  expect_equal(data2[-1, 1], as.character(1:5))
  expect_equal(data2[-1, 2], as.character(16:20))
  expect_equal(vectorize_row(data2, 1),
               c("foo", "bar", "this", "is", "a", "test"))
  expect_equal(vectorize_row(data2, 2), c("1", "16", "1", "2", "3", "4"))
})

test_that("clusters distributed in column direction can be extracted", {
  data  <- data.frame(rbind(rep(c("foo", "bar", "baz", "bum", "bup"), 2),
                            1:10, 11:20, 21:30, 31:40),
                      stringsAsFactors = FALSE)
  data2 <- unclusterize(data, regex = "foo",
                        direction = "h", pos = 1,
                        ends = list(row = "^1$|^6$", col = "bar"))
  expect_equal(data2[[1]][, 1], c("foo", 1))
  expect_equal(data2[[1]][, 2], c("bar", 2))
  expect_equal(data2[[2]][, 1], c("foo", 6))
  expect_equal(data2[[2]][, 2], c("bar", 7))

  data2 <- unclusterize(data, regex = "foo",
                         direction = "h", pos = 1,
                         offset = c(1, 1),
                         ends = list(row = "3.", col = "4|9"))
  expect_equal(data2[[1]][1, ] %>% as.numeric(), 2:4)
  expect_equal(data2[[1]][2, ] %>% as.numeric(), 12:14)
  expect_equal(data2[[2]][1, ] %>% as.numeric(), 7:9)
  expect_equal(data2[[2]][2, ] %>% as.numeric(), 17:19)


  data <- data.frame(a = rep(c("foo", "bar", "baz", "bum", "bup"), 2),
                     b = 1:10, c = 11:20,
                     stringsAsFactors = FALSE)

  data2 <- unclusterize(data, regex = "foo",
                        direction = "v", pos = 1,
                        ends = list(row = "baz", col = "11|16"))
  expect_equal(data2[[1]]$a, c("foo", "bar", "baz"))
  expect_equal(data2[[1]]$b, 1:3)
  expect_equal(data2[[1]]$c, 11:13)
  expect_equal(data2[[2]]$a, c("foo", "bar", "baz"))
  expect_equal(data2[[2]]$b, 6:8)
  expect_equal(data2[[2]]$c, 16:18)

  data2 <- unclusterize(data, regex = "foo", pos = 1,
                        direction = "v", offset = c(2, 1),
                        ends = list(row = "5|10", col = "13|18"))
  expect_equal(data2[[1]]$b, 3:5)
  expect_equal(data2[[1]]$c, 13:15)
  expect_equal(data2[[2]]$b, 8:10)
  expect_equal(data2[[2]]$c, 18:20)
})

test_that("'dim' can be controled by variable", {
  year <- 2019
  col2search <- 1
  data <- load_alldata("clustered.xlsx", sheet = "repeated")
  data2 <- unclusterize(data, regex = "year", direction = "v",
                        pos = col2search,
                        offset = c(0, 0),
                        ends = list(row = as.character(year), col = "baz"),
                        info = list(offset = c(-4, 0),
                                    dim = c(4, 2)))
  expect_equal(vectorize_row(data2[[1]], 1),
               c("year", "month", "foo", "bar", "baz",
                 "this", "is", "a", "info"))
  expect_equal(vectorize_row(data2[[1]], 2),
               as.character(c(2017, 1, 1:3, 1:4)))
  expect_equal(vectorize_row(data2[[2]], 2),
               as.character(c(2017, 1, 109:111, 5:8)))
})

test_that("extract_culsters() throws an error", {
  col2search <- 1
  data <- load_alldata("clustered.xlsx", sheet = "repeated")
  expect_error(extract_clusters(data, regex = "year", col = col2search,
                   offset = c(0, 0),
                   ends = list(row =  " ", col = "baz"),
                   info = list(offset = c(-4, 0),
                               dim = c(4, 2))))
  expect_error(extract_clusters(data, regex = "year", col = col2search,
                   offset = c(0, 0),
                   ends = list(row = "ABCDEFG", col = "baz"),
                   info = list(offset = c(-4, 0),
                               dim = c(4, 2))))
  expect_error(extract_clusters(data, regex = "year",
                   offset = c(0, 0),
                   ends = list(row =  "2019", col = "baz"),
                   info = list(offset = c(-4, 0),
                               dim = c(4, 2))))
})
