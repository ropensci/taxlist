context("Author Parsing")


test_that("Smith, Jr., John", {
  authors <- "Smith, Jr., John"
  parsed <- RefManageR:::ArrangeAuthors(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("Smith, Jr., John and Mary {Tyler Moore}", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
                  title = "An Article Title",
                  author = "Smith, Jr., John and Mary {Tyler Moore}",
                  journaltitle = "The Journal Title", date = "2014-02-06",
                  pubstate = "forthcoming")
  expect_match(bib$author[[2]]$family, "Tyler Moore")
})

test_that("Smith, Jr., John and {MATLAB, Inc.}", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
                  title = "An Article Title",
                  author = "Smith, Jr., John and {MATLAB, Inc.}",
                  journaltitle = "The Journal Title", date = "2014-02-06")
  expect_match(bib$author[[2]]$family, "MATLAB, Inc.")
  expect_equal(length(bib$author[[1]]$family), 2L)
})

test_that("Smith, John Paul and {MATLAB, Inc.}", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
          