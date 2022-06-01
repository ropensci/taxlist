# TODO:   Add comment
#
# Author: Miguel Alvarez
################################################################################

library(taxlist)
library(RPostgreSQL)

# Import data sets
data(Easplist)

conn <- dbConnect("PostgreSQL",
  dbname = "veg_databases", host = "localhost",
  port = 5432, user = "miguel", password = "kamapu"
)
Names <- dbGetQuery(conn, "SELECT * FROM tax_commons.\"taxonNames\";")

# Replace missing authors
any(is.na(Easplist@taxonNames$AuthorName))

Easplist@taxonNames$AuthorName[Easplist@taxonNames$AuthorName == "NA"] <- NA
sum(is.na(Easplist@taxonNames$AuthorName))

Names <- subset(Names, TaxonName %in% with(
  Easplist@taxonNames,
  TaxonName[is.na(AuthorName)]
))
Names <- subset(Names, !is.na(AuthorName))
Names <- subset(Names, !TaxonUsageID %in% c(10187, 205975))

Easplist@taxonNames$AuthorName <- with(
  Easplist@taxonNames,
  replace_idx(AuthorName, TaxonName, Names$TaxonName, Names$AuthorName)
)

# Duplicated name-author combinations
Easplist@taxonNames <- subset(
  Easplist@taxonNames,
  !duplicated(paste(TaxonName, AuthorName))
)

summary(Easplist)

# Finally write output
for (i in c("taxonNames", "taxonRelations", "taxonViews", "taxonTraits")) {
  write.csv2(slot(Easplist, i), paste0("data-raw/Easplist/", i, ".csv"),
    row.names = FALSE, na = ""
  )
}
