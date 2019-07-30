context("Revel using paramerer sheet")

params         <- load_param("params.csv")
colname_params <- c("object", "path", "sheet_regex",
                    "row_headers", "col_headers",
                    "cluster/dir", "cluster/pos", "cluster/regex",
                    "cluster/offset", "cluster/ends/row", "cluster/ends/col",
                    "row_type", "col_type/regex", "col_type/newname",
                    "col_type/varname", "unfiscalize/month_start",
                    "unfiscalize/rule")
colnames_report <- c(c("nrow", "ncol", "colnames",
                   gsub("/", "\\.", paste0("params.", colname_params))))

test_that("load_param loads parameter sheet correctly", {
  params <- load_param("params.csv")
  merged <- dplyr::filter(params, object == "merged")
  expect_equal(colnames(params), colname_params)
  expect_equal(merged$row_headers, "1:2")
  expect_equal(merged$col_headers, "1:2")
  expect_equal(merged$`cluster/offset`, "c(0, 0)")
})

test_that("is_csv indicates whether fname is .csv file or not", {
  expect_equal(is_csv("foo.csv"), TRUE)
  expect_equal(is_csv("foo.txt"), FALSE)
  expect_equal(is_csv("bar.csv"), TRUE)
  expect_equal(is_csv("bar.csvs"), FALSE)
  expect_equal(is_csv("bar.goodcsv"), FALSE)
})

test_that("ensure_csv retrns fname ends with '.csv'", {
  expect_equal(ensure_csv("foo.csv"), "foo.csv")
  expect_equal(ensure_csv("foo.txt"), "foo.csv")
  expect_equal(ensure_csv("foo.csvs"), "foo.csv")
  expect_equal(ensure_csv("foo.godcsv"), "foo.csv")
  expect_equal(ensure_csv(NULL), NULL)
})

test_that("mk_summary makes report from df and params", {
  res <- mk_summary(data.frame(a = 1:5, b = 6:10), param = params)
  expect_equal(colnames(res), colnames_report)
})

test_that("report returns summary", {
  df           <- data.frame(a = 1:5, b = 6:10)
  write_report <- report(df, params = params, reportf = "report.csv")
  expect_equal(report(df, reportf = NULL), df)
  expect_success(
    expect_error(
      expect_equal(report(df, reportf = "report.csv")),
      "argument \"params\" is missing, with no default"))
  expect_equal(colnames(write_report), colnames_report)
})


test_that("cat_colnames concatenates colnames of df", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_equal(cat_colnames(df), "a,b")

  df <- data.frame(a = 1:5, b = 6:10, fname = 16:20, sheet = 21:25, foo = 11:15)
  expect_equal(cat_colnames(df), "a,b,foo")
})

test_that("tbl2rebel rebels bad data driven by paramsheet", {
  res <- tbl2rebel(tbl = "params.csv", obj = "merged")
  expect_setequal(colnames(res),
                  c("NA_A2", "A1_A2", "A1_C2", "A1_D2", "E1_E2", "E1_F2",
                    "E1_G2", "E1_H2", "fname", "sheet"))
  expect_setequal(
    dplyr::pull(res, 1),
    rep(c(rep("A2", 7), rep("A10", 6), rep("A16", 8)), 2)
  )
  expect_setequal(vectorize_row(res, 1),
                  c("A2", paste0(LETTERS[c(1, 3:8)], 3),
                    "excels/merged.xlsx", "Sheet1"))
})
