context("Retrieve parents")

test_that("Parents are retrieved", {
      ID <- c(55825, 142, 52057, 56000, 53517)
      expect_equal(length(parents(Easplist, "family", ID)), 5)
      expect_equal(length(parents(Easplist, "family")),
          nrow(Easplist@taxonRelations))
    })

test_that("Errors are triggered", {
      expect_error(parents(Easplist, "suprafamily", ID))
      expect_error(parents(new("taxlist"), "family"))
      expect_error(parents(Easplist, "family", 1000000))
    })
