# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("merge taxon concepts")

test_that("taxa are merged", {
			expect_equal(nrow(Easplist@taxonRelations) >
							nrow(merge_taxa(Easplist,
											c(206, 197))@taxonRelations),
					TRUE)
			expect_is(merge_taxa(Easplist, level="species"), "taxlist")
			## expect_equal(summary(merge_taxa(Easplist,
			##                         level="species")@taxonRelations$Level)["subspecies"],
			##         0)
		}
)
