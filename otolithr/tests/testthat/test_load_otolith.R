context("Functional test")

indir      <- "../Genus-spcs"
paths      <- get_path(indir)
paths2load <- get_dir2load(paths)[1]
data       <- load_otolith(paths2load)

test_that("load_otolith() loads otolith data", {
  expect_is(data$ninc, "integer")
})
