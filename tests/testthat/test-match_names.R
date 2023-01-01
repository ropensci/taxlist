context("matching names")

test_that("match_names is working", {
  species <- c("Cperus papyrus", "Typha australis", "Luke skywalker")
  species2 <- c(
    "Apium graveolens", "Allium cepa", "Asparagus officinalis",
    "Allium sativum", "Allium cepo"
  )
  expect_is(match_names(c("Cyperus paper", "TIE fighter"), species,
    cutlevel = 0.8
  ), "data.frame")
  expect_is(match_names(species2, cutlevel = 0.8), "data.frame")
  expect_is(match_names(c("Prosopis juliflora", "Opuntia vulgaris"),
    Easplist,
    accepted_only = TRUE,
    show_concepts = TRUE
  ), "data.frame")
  expect_is(match_names(paste(
    c("Prosopis juliflora", "Opuntia vulgaris"),
    "L."
  ), Easplist, include_author = TRUE), "data.frame")
  # Test error messages
  expect_error(match_names(c("Poa pratensis", NA), Easplist))
  expect_error(match_names(c("Cyperus paper", "TIE fighter"), species,
    UsageID = 1, cutlevel = 0.8
  ))
  expect_error(match_names(c("Cyperus paper", "TIE fighter"), species,
    UsageID = rep(1, length(species)), cutlevel = 0.8
  ))
})
