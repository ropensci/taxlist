## Adding a concept
Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName),
    keep_children = TRUE)
Ebenaceae
summary(object = Ebenaceae, ConceptID = "all", maxsum = 100)

Ebenaceae <- add_concept(taxlist = Ebenaceae, TaxonName = "Euclea acutifolia",
    AuthorName = "E. Mey. ex A. DC.", Level = "species", Parent = 55707,
    ViewID = 1)

Ebenaceae
summary(Ebenaceae, "all", maxsum = 100)

# Data Frame option
Euclea <- subset(Easplist, TaxonName == "Euclea", slot = "taxonNames",
    keep_parents = TRUE)

new_species <- data.frame(TaxonName = c("Euclea sp1", "Euclea sp2"),
    Level = "species", Parent = 55707)


## Update a concept
sp_list <- Easplist
summary(sp_list, "Corchorus olitorius")
sp_list <- update_concept(taxlist = sp_list, ConceptID = 155,
    Level = "subspecies")
summary(sp_list, "Corchorus olitorius")
