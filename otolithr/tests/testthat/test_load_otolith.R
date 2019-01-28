context("Functional test")

infile   <- "../Sardinops-melanostictus_YK1508_MT6_02.hdr"
data     <- load_hdr(infile)

test_that("load_otolith() loads otolith data", {
  expect_is(load_otolith(infile), "list")
  expect_is(load_otolith(infile)$age, "numeric")
})
