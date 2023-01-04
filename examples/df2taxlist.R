Cyperus <- read.csv(file = file.path(path.package("taxlist"), "cyperus",
  "names.csv"))
head(Cyperus)

## Convert to 'taxlist' object
Cyperus$AcceptedName <- !Cyperus$SYNONYM
df2taxlist(Cyperus)

## Create a 'taxlist' object from character vectors
Plants <- df2taxlist(c("Triticum aestivum", "Zea mays"), AuthorName = "L.")
summary(Plants, "all")
