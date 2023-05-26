context("coerce objects to taxlist")

Cyperus <- read.csv(system.file("cyperus", "names.csv", package = "taxlist"),
  stringsAsFactors = FALSE
)
Cyperus$AcceptedName <- !Cyperus$SYNONYM

# system.file('dir_within_pkg', 'file_name', package = 'taxlist')

test_that("coercion to taxlist works", {
  expect_is(
    df2taxlist(Cyperus),
    "taxlist"
  )
  ## expect_is(df2taxlist(c("Triticum aestivum", "Zea mays"),
  ##   AuthorName = "L."
  ## ), "taxlist")
  ## expect_is(
  ##   df2taxlist(data.frame(
  ##     TaxonName = "Poa annua",
  ##     AuthorName = "L.",
  ##     TaxonConceptID = 1,
  ##     TaxonUsageID = 1
  ##   )),
  ##   "taxlist"
  ## )
})

## test_that("duplicated names retrieve warnings", {
##   expect_warning(df2taxlist(rep("Poa annua", 2)))
##   expect_error(df2taxlist(data.frame(
##     TaxonName = rep("Poa annua", 2),
##     AuthorName = rep("L.", 2),
##     TaxonConceptID = rep(1, 2),
##     TaxonUsageID = c(1, 2)
##   )))
## })

## test_that("error messages react", {
##   Cyperus_mod <- Cyperus[, names(Cyperus) != "TaxonConceptID"]
##   expect_error(df2taxlist(Cyperus_mod))
##   Cyperus_mod <- do.call(rbind, list(Cyperus, Cyperus[1, ]))
##   expect_error(df2taxlist(Cyperus_mod))
##   Cyperus_mod <- Cyperus
##   Cyperus_mod$TaxonConceptID[2] <- Cyperus_mod$TaxonConceptID[1]
##   expect_error(df2taxlist(Cyperus_mod))
## })
