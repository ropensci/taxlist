# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("manipulating taxon concepts")

test_that("function taxon_names is working", {
			expect_is(taxon_relations(Easplist), "data.frame")
		}
)

test_that("function add_concept is working", {
			expect_equal(nrow(Easplist@taxonRelations) <
							nrow(add_concept(Easplist,
											TaxonName="Euclea acutifolia",
											AuthorName="E. Mey. ex A. DC.",
											Level="species", Parent=55707,
											ViewID=1)@taxonRelations),
					TRUE)
		}
)

test_that("function update_concept is working", {
			expect_equal(with(update_concept(Easplist, 155,
									Level="subspecies")@taxonRelations,
							paste(Level[TaxonConceptID == 155])), "subspecies")
		}
)
