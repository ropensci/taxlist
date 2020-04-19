context("counting taxa")

test_that("count_taxa is working", {
			expect_equal(count_taxa(iris$Species), length(unique(iris$Species)))
			expect_equal(count_taxa(Easplist), nrow(Easplist@taxonRelations))
			expect_equal(count_taxa(Easplist, level="species"),
					with(Easplist@taxonRelations,
							length(Level[paste(Level) == "species"])))
			expect_error(count_taxa(Easplist, level="superspecies"))
			Easplist <- tax2traits(Easplist, get_names=TRUE)
			expect_is(count_taxa(~ lf_behn_2018 + family, Easplist),
					"data.frame")
			expect_is(count_taxa(~ lf_behn_2018 + family, Easplist,
							include_na=TRUE), "data.frame")
		}
)
