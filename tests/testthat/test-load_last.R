context("loading a backup")

test_that("load_last is working", {
  pat <- system.file("extdata",
    "Podocarpus_2020-01-10.rda",
    package = "taxlist"
  )

  # remove date stamp - that's what the function will append automatically.

  use_pat <- gsub("_[0-9]+-[0-9]+-[0-9]+\\.rda$", "", pat)

  expect_is(
    {
      load_last(use_pat)
      Podocarpus
    },
    "taxlist"
  )
})
