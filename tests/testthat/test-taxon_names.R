context("manipulating taxon usage names")

test_that("function taxon_names is working", {
  expect_is(taxon_names(Easplist), "data.frame")
  Euclea <- subset(Easplist, charmatch("Euclea", TaxonName),
    keep_children = TRUE
  )
  Names_Euclea <- taxon_names(Euclea)
  Names_Euclea$AuthorName <- "me"
  expect_equal(
    all(({
      taxon_names(Euclea) <- Names_Euclea
      Euclea@taxonNames$AuthorName
    }) == "me"),
    TRUE
  )
})

test_that("function add_synonym is working", {
  expect_equal(
    nrow(Easplist@taxonNames) <
      nrow(add_synonym(Easplist, 51793,
        TaxonName = "Maba scabra",
        AuthorName = "Chiov."
      )@taxonNames),
    TRUE
  )
  expect_error(add_synonym(Easplist, max(Easplist$TaxonConceptID) +
    10,
  TaxonName = "new name", AuthorName = "NN."
  ))
})

test_that("function update_name is working", {
  expect_equal(with(
    update_name(Easplist, 51793,
      TaxonName = "changed"
    )@taxonNames,
    TaxonName[TaxonUsageID == 51793]
  ), "changed")
})

test_that("function delete_name is working", {
  expect_equal(nrow(Easplist@taxonNames) > nrow(delete_name(
    Easplist,
    53821
  )@taxonNames), TRUE)
})
