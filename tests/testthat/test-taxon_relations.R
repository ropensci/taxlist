context("manipulating taxon concepts")

# Loading installed examples
pat <- system.file("taxlist_examples", "examples.Rda", package = "taxlist")

load(pat)

test_that("function taxon_relations is working", {
			expect_is(taxon_relations(Easplist), "data.frame")
			tmp <- Easplist
			tmp_rel <- Easplist@taxonRelations
			tmp_rel$ViewID <- NA
			expect_equal(all(is.na(({taxon_relations(tmp) <- tmp_rel
												tmp@taxonRelations$ViewID}))),
					TRUE)
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
			Euclea <- subset(Easplist, grepl("Euclea", TaxonName),
					slot="names", keep_parents=TRUE)
			Diospyros <- subset(Easplist, grepl("Diospyros", TaxonName),
					slot="names")
			expect_is(add_concept(Diospyros, Euclea), "taxlist")
			# Same family in both taxlist objects retrieves error
			Diospyros <- subset(Easplist, grepl("Diospyros", TaxonName),
					slot="names", keep_parents=TRUE)
			expect_error(add_concept(Diospyros, Euclea))
			# Working with lists without information on taxonomic levels
			expect_equal(nrow(add_concept(data2,
								TaxonName="Planta incognita")@taxonRelations) >
					nrow(data2@taxonRelations), TRUE)
			expect_warning(add_concept(data2, TaxonName="Planta incognita",
							Level="species"))
			# Adding on a new object
			expect_equal(nrow(add_concept(new("taxlist"),
								TaxonName="Planta incognita")@taxonRelations),
					1)
			# Addition including taxon view
			tax_1 <- clean(subset(Easplist, TaxonName == "Cyperus papyrus",
							slot="names"))
			tax_1@taxonViews <- tax_1@taxonViews[tax_1@taxonViews$ViewID %in%
							tax_1@taxonRelations$ViewID, ]
			tax_2 <- clean(subset(Easplist, TaxonName == "Cyperaceae",
							slot="names"))
			tax_2@taxonViews <- tax_2@taxonViews[tax_2@taxonViews$ViewID %in%
							tax_2@taxonRelations$ViewID, ]
			expect_is(add_concept(tax_1, tax_2, insert_view=TRUE), "taxlist")
		}
)

test_that("function update_concept is working", {
			expect_equal(with(update_concept(Easplist, 155,
									Level="subspecies")@taxonRelations,
							paste(Level[TaxonConceptID == 155])), "subspecies")
		}
)
