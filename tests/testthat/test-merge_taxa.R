context("merge taxon concepts")

test_that("taxa are merged", {
  expect_equal(
    nrow(Easplist@taxonRelations) >
      nrow(merge_taxa(
        Easplist,
        c(206, 197)
      )@taxonRelations),
    TRUE
  )
  expect_is(
    merge_taxa(Easplist, level = "species"),
    "taxlist"
  )
  result <- evaluate_promise(merge_taxa(Easplist, c(206, 197),
    print_output = TRUE
  ), print = TRUE)
  expect_equal(grepl("Cyperus", result$output), TRUE)
})

test_that("Error messages are triggered", {
  expect_error(merge_taxa(Easplist, concepts = 1))
  expect_error(merge_taxa(Easplist, concepts = c("flora", "fauna")))
  expect_error(merge_taxa(Easplist))
})
