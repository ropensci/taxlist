
context("importing Turboveg data sets")

test_that("import from Turboveg is working", {
  pat <- system.file("tv_data", package = "taxlist")
  expect_is(tv2taxlist("cyperus", pat), "taxlist")
})
