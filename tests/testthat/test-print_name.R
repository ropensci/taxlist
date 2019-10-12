# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("printing taxon usage names")

test_that("printed names are working", {
			expect_is(print_name(Easplist, 363, style="expression"),
					"expression")
			expect_is(print_name(Easplist, 363, style="expression",
							secundum="secundum"), "expression")
			expect_equal(print_name(Easplist, 363),
					"*Ludwigia adscendens* ssp. *diffusa* (Forssk.) P.H. Raven")
			expect_equal(print_name(Easplist, 363, style="html"),
					"<i>Ludwigia adscendens</i> ssp. <i>diffusa</i> (Forssk.) P.H. Raven")
			expect_equal(print_name(Easplist, 363, style="knitr"),
					"\\textit{Ludwigia adscendens} ssp. \\textit{diffusa} (Forssk.) P.H. Raven")
		}
)
