# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("manipulating taxon traits")

test_that("function taxon_traits is working", {
			expect_is(taxon_traits(Easplist), "data.frame")
			expect_error(taxon_traits(Easplist) <- iris)
			tmp <- Easplist
			tmp_traits <- Easplist@taxonTraits[1:10,]
			expect_equal(nrow({taxon_traits(tmp) <- tmp_traits
							taxon_traits(tmp)}) == nrow(tmp_traits), TRUE)
		}
)
