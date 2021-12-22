context("accepted name manipulation")

test_that("accepted_name is working", {
  expect_is(accepted_name(Easplist), "data.frame")
  expect_equal(
    nrow(accepted_name(Easplist)),
    nrow(Easplist@taxonRelations)
  )
})

test_that("accepted_name replacement is working", {
  expect_error(accepted_name(Easplist, 50074) <- 51129)
  expect_error(accepted_name(Easplist, c(68, 50074)) <- 56139)
  tmp <- add_synonym(Easplist, 68,
    TaxonName = "Basella cordifolia",
    AuthorName = "Lam."
  )
  expect_equal(with(
    tmp@taxonRelations,
    AcceptedName[TaxonConceptID == 68]
  ) != {
    accepted_name(tmp, 68) <- 56139
    with(
      tmp@taxonRelations,
      AcceptedName[TaxonConceptID == 68]
    )
  }, TRUE)
})

test_that("basionym is working", {
  expect_is(basionym(Easplist), "data.frame")
  expect_equal(
    nrow(basionym(Easplist)),
    nrow(Easplist@taxonRelations)
  )
  expect_equal(
    {
      basionym(Easplist, 50074) <- 53097
      with(
        Easplist@taxonRelations,
        Basionym[TaxonConceptID == 50074]
      )
    },
    53097
  )
  expect_is(
    {
      basionym(Easplist, 50074) <- 53097
      basionym(Easplist, 50074)
    },
    "data.frame"
  )
  expect_equal(
    {
      basionym(Easplist, 50074) <- 53097
      basionym(Easplist, 50074)$Basionym
    },
    with(
      Easplist@taxonRelations,
      Basionym[TaxonConceptID == 50074]
    )
  )
  expect_error(basionym(Easplist, 50074) <- 51756)
  expect_error(basionym(Easplist, 50074) <- c(53097, 53100))
})
