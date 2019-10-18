# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("importing Turboveg data sets")

test_that("import from Turboveg is working", {
			expect_is(tv2taxlist("cyperus", file.path(path.package("taxlist"),
									"tv_data")), "taxlist")
		}
)
