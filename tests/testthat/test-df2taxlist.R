# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

context("coerce objects to taxlist")

Cyperus <- read.csv(file.path(path.package("taxlist"), "cyperus", "names.csv"),
		stringsAsFactors=FALSE)

test_that("coercion to taxlist works", {
			expect_is(df2taxlist(Cyperus, AcceptedName=!Cyperus$SYNONYM),
					"taxlist")
			expect_is(df2taxlist(c("Triticum aestivum","Zea mays"),
							AuthorName="L."), "taxlist")
		}
)
