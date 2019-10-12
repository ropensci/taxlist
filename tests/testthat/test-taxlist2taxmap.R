# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("conversion to taxa")

test_that("conversion to taxa is working", {
			expect_equal(class(taxlist2taxmap(Easplist)),
					c("Taxmap", "Taxonomy", "R6"))
		}
)
