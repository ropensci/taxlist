context("Merge taxa to their respective parents.")

test_that("Taxa gets merged to their parents.", {
  expect_is(merge_to_parent(Easplist, c(346, 50400)), "taxlist")
  expect_error(merge_to_parent(Easplist, c(346, 50400, 54919)))
})
