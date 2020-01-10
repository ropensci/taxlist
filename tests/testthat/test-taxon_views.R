# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("manipulating taxon views")

test_that("function taxon_views is working", {
			expect_is(taxon_views(Easplist), "data.frame")
			tmp <- Easplist
			tmp@taxonRelations$ViewID <- NA
			expect_equal(nrow({taxon_views(tmp) <- data.frame()
							tmp@taxonViews}), 0)
		}
)

test_that("function add_view is working", {
			expect_is(add_view(Easplist, secundum="Beentje et al. (1952)",
							Title="Flora of Tropical East Africa",
							URL=file.path("http://www.kew.org/science",
									"directory/projects",
									"FloraTropEAfrica.html")),
					"taxlist")
			expect_equal(nrow(Easplist@taxonViews) < nrow(add_view(Easplist,
							secundum="Beentje et al. (1952)",
							Title="Flora of Tropical East Africa")@taxonViews),
					TRUE)
			tmp <- Easplist
			tmp@taxonRelations$ViewID <- NA
			tmp@taxonViews <- data.frame()
			expect_is(add_view(tmp, secundum="Beentje et al. (1952)",
							Title="Flora of Tropical East Africa",
							URL=file.path("http://www.kew.org/science",
								"directory/projects/FloraTropEAfrica.html")),
					"taxlist")
			expect_equal(nrow(add_view(tmp, secundum="Beentje et al. (1952)",
							Title="Flora of Tropical East Africa")@taxonViews),
					1)
		}
)
