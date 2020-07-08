context("manipulating taxon traits")

test_that("function taxon_traits is working", {
			expect_is(taxon_traits(Easplist), "data.frame")
			expect_error(taxon_traits(Easplist) <- iris)
			tmp <- Easplist
			tmp_traits <- Easplist@taxonTraits[1:10, ]
			expect_equal(nrow({taxon_traits(tmp) <- tmp_traits
							taxon_traits(tmp)}) == nrow(tmp_traits), TRUE)
			tax_1 <- df2taxlist(data.frame(TaxonName="Poa annua",
							AuthorName="L.", TaxonConceptID=1, TaxonUsageID=1),
					AcceptedName=TRUE)
			expect_equal({taxon_traits(tax_1) <- data.frame(TaxonConceptID="1",
								lf="grass")
						nrow(tax_1@taxonTraits)
					}, 1)
			expect_warning(taxon_traits(tax_1) <- data.frame(
								TaxonConceptID=rep(1, 2),
								lf=c("grass", "herb")))
	}
)
