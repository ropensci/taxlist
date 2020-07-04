context("coerce objects to taxlist")

Cyperus <- read.csv(file.path(path.package("taxlist"), "cyperus", "names.csv"),
        stringsAsFactors=FALSE)

system.file('dir_within_pkg', 'file_name', package = 'taxlist')

test_that("coercion to taxlist works", {
			expect_is(df2taxlist(Cyperus, AcceptedName=!Cyperus$SYNONYM),
					"taxlist")
			expect_is(df2taxlist(c("Triticum aestivum","Zea mays"),
							AuthorName="L."), "taxlist")
			expect_is(df2taxlist(data.frame(
									TaxonName="Poa annua",
									AuthorName="L.",
									TaxonConceptID=1,
									TaxonUsageID=1),
							AcceptedName=TRUE,
							url=file.path("http://www.theplantlist.org/tpl1.1",
									"record/kew-435194")),
					"taxlist")
		}
)

test_that("duplicated names retrieve warnings", {
			expect_warning(df2taxlist(rep("Poa annua", 2)))
			expect_warning(df2taxlist(data.frame(
									TaxonName=rep("Poa annua", 2),
									AuthorName=rep("L.", 2),
									TaxonConceptID=rep(1, 2),
									TaxonUsageID=c(1, 2)),
							AcceptedName=TRUE))
		}
)

test_that("error messages work properly", {
			expect_error(df2taxlist(data.frame(TaxonName="Poa annua",
									AuthorName="L.", TaxonConceptID=1,
									TaxonUsageID=1), AcceptedName=rep(TRUE,2)))
			expect_error(df2taxlist(data.frame(
									TaxonName=c("Poa annua", "Poa pratensis"),
									AuthorName=rep("L.", 2),
									TaxonConceptID=c(1, 2),
									TaxonUsageID=rep(1, 2)),
							AcceptedName=rep(TRUE,2)))
		})
