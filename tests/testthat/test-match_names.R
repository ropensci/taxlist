# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("matching names")

test_that("match_names is working", {
			expect_is(match_names(c("Prosopis juliflora","Opuntia vulgaris"),
							Easplist), "data.frame")
			expect_equal(nrow(match_names(c("Prosopis juliflora",
											"Opuntia vulgaris"), Easplist)), 2)
			expect_is(match_names(c("Prosopis juliflora","Opuntia vulgaris"),
							Easplist, output="list"), "list")
			expect_equal(length(match_names(c("Prosopis juliflora",
											"Opuntia vulgaris"), Easplist,
									output="list")), 2)
		}
)
