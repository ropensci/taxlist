# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("manipulating taxon traits")

test_that("function taxon_names is working", {
			expect_is(taxon_traits(Easplist), "data.frame")
		}
)
