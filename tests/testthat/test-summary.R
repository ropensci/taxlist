context("Testing summary outputs")

test_that("Summaries are produced", {
  result <- evaluate_promise(summary(Easplist), print = TRUE)
  expect_true(grepl("object", result$output))
  result <- evaluate_promise(summary(Easplist, "Cyperus pap"),
    print = TRUE
  )
  expect_true(grepl("Cyperus", result$output))
  expect_error(summary(Easplist, 1000000000))
  expect_error(summary(Easplist, 1, display = "artist"))
})
