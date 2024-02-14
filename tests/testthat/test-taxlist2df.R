context("coerce taxlist objects to data frame")

test_that("coercion to data frame works", {
  tax <- Easplist
  tax@taxonNames$Basionym <- NA
  tax@taxonTraits$Basionym <- NA
  tax@taxonViews$Basionym <- NA
  expect_is(
    as(tax, "data.frame"),
    "data.frame"
  )
  expect_is(
    taxlist2df(tax, standard = "dwc"),
    "data.frame"
  )
  expect_error(
    taxlist2df(tax, standard = "something")
  )
})
