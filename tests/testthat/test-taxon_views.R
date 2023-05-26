context("manipulating taxon views")

test_that("function taxon_views is working", {
  expect_is(taxon_views(Easplist), "data.frame")
  tmp <- Easplist
  tmp@taxonRelations$ViewID <- NA
  expect_equal(nrow({
    taxon_views(tmp) <- data.frame(ViewID = integer(0))
    tmp@taxonViews
  }), 0)
})

test_that("function add_view is working", {
  expect_is(
    add_view(
      Easplist,
      data.frame(
        secundum = "Beentje et al. (1952)",
        Title = "Flora of Tropical East Africa",
        URL = file.path(
          "http://www.kew.org/science",
          "directory/projects",
          "FloraTropEAfrica.html"
        )
      )
    ),
    "taxlist"
  )
  expect_equal(
    nrow(Easplist@taxonViews) < nrow(add_view(
      Easplist,
      data.frame(
        secundum = "Beentje et al. (1952)",
        Title = "Flora of Tropical East Africa"
      )
    )@taxonViews),
    TRUE
  )
  # TODO: Solve next example
  ## tmp <- Easplist
  ## tmp@taxonRelations$ViewID <- NA
  ## tmp@taxonViews <- data.frame()
  ## expect_is(
  ##   add_view(tmp,
  ##     data.frame(secundum = "Beentje et al. (1952)",
  ##     Title = "Flora of Tropical East Africa",
  ##     URL = file.path(
  ##       "http://www.kew.org/science",
  ##       "directory/projects/FloraTropEAfrica.html"
  ##     ))
  ##   ),
  ##   "taxlist"
  ## )
  # TODO: Solve next example
  ## expect_equal(
  ##   nrow(add_view(tmp,
  ##     data.frame(secundum = "Beentje et al. (1952)",
  ##     Title = "Flora of Tropical East Africa")
  ##   )@taxonViews),
  ##   1
  ## )
})
