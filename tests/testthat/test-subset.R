context("producing subsets")

test_that("function subset", {
  expect_error(subset(Easplist, lf_behn_2018 == "reed_plant",
    slot = "animals"
  ))
  expect_equal(nrow(Easplist@taxonRelations) >
    nrow(subset(Easplist, ViewID == 1,
      slot = "views"
    )@taxonRelations), TRUE)
  expect_equal(nrow(subset(
    Easplist,
    TaxonName == "Poaceae"
  )@taxonRelations) <
    nrow(subset(Easplist, TaxonName == "Poaceae",
      keep_children = TRUE
    )@taxonRelations), TRUE)
})
