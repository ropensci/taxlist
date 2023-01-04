context("write indented lists")

test_that("Indented lists are produced", {
  result <- evaluate_promise(indented_list(Easplist), print = TRUE)
  expect_true(grepl("Hallea", result$output))
  result <- evaluate_promise(
    indented_list(Easplist,
      level = TRUE, synonyms = TRUE, seucundum = "secundum"
    ),
    print = TRUE
  )
  expect_true(grepl("Hallea", result$output))
})
