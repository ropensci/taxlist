context("dissecting names")

tmp <- Easplist@taxonNames[1:15, "TaxonName"]

test_that("proper output of dissect name", {
  expect_is(dissect_name(tmp), "matrix")
  expect_equal(nrow(dissect_name(tmp)), length(tmp))
  expect_equal(length(dissect_name(tmp, repaste = c(1:2))), length(tmp))
})
