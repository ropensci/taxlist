# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("manipulating taxon views")

test_that("function taxon_views is working", {
			expect_is(taxon_views(Easplist), "data.frame")
		}
)

test_that("function add_view is working", {
			expect_is(taxon_views(Easplist), "data.frame")
		}
)
