context("adding suffix to a list")

test_that("suffix addition is working", {
  expect_equal(taxlist:::add_suffix("x", "y"), "x")
  expect_equal(taxlist:::add_suffix("x", "x"), "x_1")
})
