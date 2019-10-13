# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("manipulating taxon views")

test_that("function taxon_views is working", {
			expect_is(taxon_views(Easplist), "data.frame")
		}
)

test_that("function add_view is working", {
			expect_is(add_view(Easplist, secundum="Beentje et al. (1952)",
							Title="Flora of Tropical East Africa",
							URL="http://www.kew.org/science/directory/projects/FloraTropEAfrica.html"),
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
							URL="http://www.kew.org/science/directory/projects/FloraTropEAfrica.html"),
					"taxlist")
			expect_equal(nrow(add_view(tmp, secundum="Beentje et al. (1952)",
									Title="Flora of Tropical East Africa")@taxonViews),
					1)
		}
)
