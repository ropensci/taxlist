# TODO:   Test function accepted_name
# 
# Author: Miguel Alvarez
################################################################################

context("accepted name manipulation")

test_that("accepted_name is working", {
			expect_is(accepted_name(Easplist), "data.frame")
			expect_equal(nrow(accepted_name(Easplist)),
					nrow(Easplist@taxonRelations))
		}
)

test_that("accepted_name replacement is working", {
			expect_error(accepted_name(Easplist, 50074) <- 51129)
			expect_error(accepted_name(Easplist, c(68, 50074)) <- 56139)
			tmp <- add_synonym(Easplist, 68, TaxonName="Basella cordifolia",
					AuthorName="Lam.")
			expect_equal(with(tmp@taxonRelations,
							AcceptedName[TaxonConceptID == 68]) != {
						accepted_name(tmp, 68) <- 56139
						with(tmp@taxonRelations,
								AcceptedName[TaxonConceptID == 68])
					}, TRUE)
		}
)
