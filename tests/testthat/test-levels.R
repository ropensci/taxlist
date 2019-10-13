# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("retrieving levels")

test_that("levels in taxlist object", {
			expect_equal(levels(Easplist),
					base::levels(Easplist@taxonRelations$Level))
			expect_equal(length(levels(Easplist)) < {
						levels(Easplist) <- c(levels(Easplist), "phylum")
						length(levels(Easplist))
					}, TRUE)
		}
)
