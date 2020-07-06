context("extract rows or columns")

test_that("square brackets work", {
			expect_is(Easplist[1, ], "taxlist")
			expect_is(Easplist[c(1:10), ], "taxlist")
			expect_is(Easplist[-c(1:5), ], "taxlist")
			expect_equal(nrow(Easplist[1, ]@taxonRelations), 1)
			expect_is(Easplist[ ,1], "taxlist")
			expect_is(Easplist[ ,c(1:2)], "taxlist")
			expect_is(Easplist[ ,-c(1:2)], "taxlist")
			expect_equal(ncol(Easplist[ ,1]@taxonTraits), 1)
		}
)

test_that("dollar works", {
			expect_equal(Easplist$TaxonConceptID,
					Easplist@taxonRelations$TaxonConceptID)
			## expect_equal(Easplist$lf_behn_2018,
			##         Easplist@taxonTraits$lf_behn_2018)
			expect_equal(length(Easplist$lf_behn_2018),
					nrow(Easplist@taxonRelations))
		}
)
