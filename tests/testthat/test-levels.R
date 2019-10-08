# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("retrieving levels")

test_that("levels in taxlist object", {
			expect_equal(levels(Easplist),
					base::levels(Easplist@taxonRelations$Level))
		}
)
