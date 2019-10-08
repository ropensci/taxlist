# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("extract rows or columns")

test_that("dollar works", {
			expect_equal(Easplist$lf_behn_2018,
					Easplist@taxonTraits$lf_behn_2018)
		}
)

# TODO: Apparently the second index is not working
test_that("square brackets work", {
			expect_is(Easplist[1,], "taxlist")
			expect_equal(nrow(Easplist[1,]@taxonRelations), 1)
		}
)
