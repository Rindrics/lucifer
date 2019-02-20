context("Locate cell position")
library(tibble)
df <- tribble(~A, ~B, ~C, ~D,
              NA, NA, NA, 1,
              NA, "name", "value", 2,
              "", "foo", 12, 3,
              "", "bar", 123, 4,
              "", "baz", 1234, 5,
              "", "bum", 12345, 6,
              "", "foo", 12, 7)

test_that("quot_ring() throws value on given quotient ring", {
  expect_equal(quot_ring(0, 3), 3)
  expect_equal(quot_ring(1, 3), 1)
  expect_equal(quot_ring(2, 3), 2)
  expect_equal(quot_ring(0, 5), 5)
  expect_equal(quot_ring(1, 5), 1)
  expect_equal(quot_ring(2, 5), 2)
  expect_equal(quot_ring(3, 5), 3)
  expect_equal(quot_ring(4, 5), 4)
})

test_that("quot2col() throws col position from
 quotient of given match position in matrix", {
  expect_equal(quot2col(3, 0), 3)
  expect_equal(quot2col(3, 1), 4)
  expect_equal(quot2col(4, 1), 5)
  expect_equal(quot2col(5, 1), 6)
  expect_equal(quot2col(5, 2), 6)
})

test_that("get_locate_patterns() locates cell position that has given regex", {
  expect_equal(locate_patterns(df, "name"), "$B$2")
  expect_equal(locate_patterns(df, "nam."), "$B$2")
  expect_equal(locate_patterns(df, "foo"), c("$B$3", "$B$7"))
})

test_that("get_topleft() locates cell position that has given regex", {
  expect_equal(get_topleft(df, "name"), "$B$2")
  expect_equal(get_topleft(df, "nam."), "$B$2")
  expect_equal(get_topleft(df, "foo"), "$B$3")
})

test_that("get_bottomright() locates cell position that has given regex", {
  expect_equal(get_bottomright(df, "12345"), "$C$6")
  expect_equal(get_bottomright(df, "foo"), "$B$7")
})
