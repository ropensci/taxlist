context("Testing summary outputs")

test_that("Summaries are produced", {
  result <- evaluate_promise(summary(Easplist), print = TRUE)
  expect_equal(grepl("object", result$output), TRUE)
  result <- evaluate_promise(summary(Easplist, "Cyperus pap"),
    print = TRUE
  )
  expect_equal(grepl("Cyperus", result$output), TRUE)
  expect_error(summary(Easplist, 1000000000))
  expect_error(summary(Easplist, 1, display = "artist"))
})
