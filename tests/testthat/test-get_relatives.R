# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("recover children or parents")

Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName))
Diostri <- subset(Easplist, TaxonName == "Diospyros tricolor")

test_that("get_children works", {
			expect_true(nrow(Ebenaceae@taxonRelations) <
							nrow(get_children(Easplist,
											Ebenaceae)@taxonRelations))
		}
)

test_that("get_parents works", {
			expect_true(nrow(Diostri@taxonRelations) <
							nrow(get_parents(Easplist,
											Diostri)@taxonRelations))
		}
)
