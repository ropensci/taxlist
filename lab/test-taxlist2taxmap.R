context("converting taxlist objects into taxmap")

test_that("conversion to taxmap is working", {
  Cyperus <- subset(Easplist, grepl("Cyperus", TaxonName))
  Cyperus <- taxlist2taxmap(Cyperus)
  expect_is(Cyperus, "Taxmap")
  expect_is(taxmap2taxlist(Cyperus,
    relations = "relations",
    traits = "traits", synonyms = "synonyms",
    views = "views"
  ), "taxlist")
})
