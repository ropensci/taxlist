# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("counting taxa")

test_that("count taxa is working", {
			expect_equal(count_taxa(iris$Species), length(unique(iris$Species)))
			expect_equal(count_taxa(Easplist), nrow(Easplist@taxonRelations))
		}
)
