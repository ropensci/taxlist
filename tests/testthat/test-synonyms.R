context("synonyms addition")

spp <- df2taxlist("Poa annua", AuthorName="L.")

test_that("number of synonyms are congruent", {
			expect_equal(nrow(synonyms(Easplist)),
					nrow(Easplist@taxonNames) - nrow(Easplist@taxonRelations))
			expect_is(synonyms(Easplist, 1), "data.frame")
		}
)
