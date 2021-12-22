context("matching names")

test_that("match_names is working", {
  expect_is(match_names(c("Prosopis juliflora", "Opuntia vulgaris"),
    Easplist,
    accepted_only = TRUE,
    show_concepts = TRUE
  ), "data.frame")
  expect_equal(nrow(match_names(c(
    "Prosopis juliflora",
    "Opuntia vulgaris"
  ), Easplist)), 2)
  expect_is(match_names(c("Prosopis juliflora", "Opuntia vulgaris"),
    Easplist,
    output = "list"
  ), "list")
  expect_equal(length(match_names(c(
    "Prosopis juliflora",
    "Opuntia vulgaris"
  ), Easplist,
  output = "list"
  )), 2)
  expect_equal(nrow(match_names(
    "Poa pratensis",
    c("Poa annua", "Geranium pratensis")
  )), 2)
  expect_warning(match_names(
    c("Geranium robertianum", "Poa pratensis"),
    c("Poa annua", "Geranium pratensis")
  ))
})
