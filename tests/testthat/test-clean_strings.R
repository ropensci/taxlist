context("cleaning strings")

test_that("clean_strings is working", {
  expect_is(clean_strings(Easplist@taxonNames$TaxonName), "character")
  expect_is(clean_strings(Easplist@taxonRelations$Level), "factor")
  expect_is(clean_strings(Easplist@taxonNames), "data.frame")
  expect_equal(clean_strings("  Daucus  carota "), "Daucus carota")
})
