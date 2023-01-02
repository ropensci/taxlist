context("coercion to list")

test_that("coercion to list works", {
  expect_is(as(Easplist, "list"), "list")
})
