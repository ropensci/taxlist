# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("inserting taxonomic information in slot taxonTraits")

test_that("comparing columns in taxon traits", {
			expect_equal(ncol(Easplist@taxonTraits) <
							ncol(tax2traits(Easplist)@taxonTraits), TRUE)
		}
)
