# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("change a name to a different concept")

tmp <- subset(Easplist, TaxonConceptID %in% c(50105, 573), slot="relations")

test_that("assigning different concepts works", {
			expect_equal(with(tmp@taxonNames,
									length(TaxonName[TaxonConceptID == 573])) <
							{
								change_concept(tmp, 53130) <- 573
								with(tmp@taxonNames,
										length(TaxonName[TaxonConceptID == 573]))
							}, TRUE)
			expect_error(change_concept(tmp, 50105) <- 573)
			expect_error(change_concept(tmp, 50105) <- c(573, 50105))
		}
)
