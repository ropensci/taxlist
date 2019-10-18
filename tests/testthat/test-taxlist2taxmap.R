# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("conversion to taxa")

load(file.path(path.package("taxlist"), "taxlist_examples/examples.Rda"))
data1 <- taxlist2taxmap(data1)

test_that("conversion to taxa is working", {
			expect_equal(class(taxlist2taxmap(Easplist)),
					c("Taxmap", "Taxonomy", "R6"))
			expect_equal(class(taxmap2taxlist(data1, relations="relations",
									traits="traits", synonyms="synonyms", 
									views="views")), "taxlist")
		}
)
