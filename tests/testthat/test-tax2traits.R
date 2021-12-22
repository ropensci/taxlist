context("inserting taxonomic information in slot taxonTraits")

test_that("function tax2traits", {
  expect_equal(ncol(Easplist@taxonTraits) <
    ncol(tax2traits(Easplist)@taxonTraits), TRUE)
  tmp <- Easplist
  tmp@taxonRelations$Level <- NA
  expect_error(tax2traits(tmp))
  tmp <- Easplist
  tmp@taxonRelations$Parent <- NA
  expect_error(tax2traits(tmp))
  expect_is(tax2traits(Easplist)$family, "integer")
  expect_is(tax2traits(Easplist, get_names = TRUE)$family, "character")
})
