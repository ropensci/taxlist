# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("merge taxon concepts")

tmp <- subset(Easplist, TaxonConceptID %in% c(206,197), slot="relations")

test_that("taxa are merged", {
			expect_equal(nrow(tmp@taxonRelations), 2)
			expect_equal(nrow(merge_taxa(tmp, c(206, 197))@taxonRelations), 1)
		}
)
