# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("ropensci/taxlist", "issue-27")

library(taxlist)

## Read the table with names of Cyperus species
Cyperus <- read.csv(file = file.path(path.package("taxlist"), "cyperus",
  "names.csv"))
head(Cyperus)

## Convert to 'taxlist' object
Cyperus$AcceptedName <- !Cyperus$SYNONYM
Cyperus <- df2taxlist(Cyperus)
Cyperus

## Create a 'taxlist' object from character vectors
Plants <- df2taxlist(c("Triticum aestivum", "Zea mays"), AuthorName = "L.")
summary(Plants, "all")

# For tv2taxlist
library(taxlist)
library(foreign)

taxlist = "cyperus"
tv_home = file.path(path.package("taxlist"), "tv_data")




