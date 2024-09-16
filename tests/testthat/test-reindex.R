context("Re-index taxlist objects.")

test_that("Re-index is working.", {
  sp_list <- Easplist
  expect_is(
    {
      reindex(sp_list) <- id_generator(nrow(sp_list@taxonRelations),
        mode = "character"
      )
      sp_list@taxonRelations$TaxonConceptID
    },
    "character"
  )
  expect_is(
    {
      reindex(sp_list, idx = "TaxonUsageID") <- id_generator(nrow(sp_list@taxonNames),
        mode = "character"
      )
      sp_list@taxonNames$TaxonUsageID
    },
    "character"
  )
  expect_is(
    {
      reindex(sp_list, idx = "ViewID") <- id_generator(nrow(sp_list@taxonViews),
        mode = "character"
      )
      sp_list@taxonViews$ViewID
    },
    "character"
  )
})

test_that("Error messages work.", {
  sp_list <- Easplist
  expect_error(reindex(sp_list, idx = "bibtexkey") <- id_generator(nrow(sp_list@taxonRelations),
    mode = "character"
  ))
  expect_error(reindex(sp_list) <- rep(10, nrow(sp_list@taxonRelations)))
  expect_error(reindex(sp_list, idx = "usage") <- rep(10, nrow(sp_list@taxonNames)))
  expect_error(reindex(sp_list, idx = "view") <- rep(10, nrow(sp_list@taxonViews)))
})
