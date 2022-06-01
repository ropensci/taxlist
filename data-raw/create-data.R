# TODO:   Script for generating data sets in package 'taxlist'
#
# Author: Miguel Alvarez
################################################################################

library(taxlist)

# Easplist ---------------------------------------------------------------------


# Sub-folder in taxlist source
Dir <- "data-raw/Easplist"

Easplist <- new("taxlist")

# Read tables
for (i in c("taxonViews", "taxonRelations", "taxonNames", "taxonTraits")) {
  slot(Easplist, i) <- read.csv2(file.path(Dir, paste0(i, ".csv")),
    encoding = "UTF-8", stringsAsFactors = FALSE
  )
}
levels(Easplist) <- c(
  "form", "variety", "subspecies", "species", "complex", "genus",
  "family"
)

# Cross-check
summary(Easplist)

# Write the object
save(Easplist, file = "data/Easplist.rda")
