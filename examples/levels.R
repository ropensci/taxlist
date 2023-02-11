## Get levels of species list
levels(Easplist)

## Add aggregate as new taxonomic level
levels(Easplist) <- c("form", "variety", "subspecies", "species", "complex",
    "aggregate", "genus", "family")
summary(Easplist)
