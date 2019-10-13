# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("counting taxa")

test_that("count_taxa is working", {
			expect_equal(count_taxa(iris$Species), length(unique(iris$Species)))
			expect_equal(count_taxa(Easplist), nrow(Easplist@taxonRelations))
			expect_equal(count_taxa(Easplist, "species"),
					with(Easplist@taxonRelations,
							length(Level[paste(Level) == "species"])))
			expect_error(count_taxa(Easplist, "superspecies"))
		}
)
