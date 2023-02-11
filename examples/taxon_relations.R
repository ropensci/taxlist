## Subset for the genus Euclea and display of slot 'taxonNames'
Euclea <- subset(x = Easplist, subset = charmatch("Euclea", TaxonName),
  slot = "names", keep_children = TRUE)
Euclea
taxon_relations(Euclea)

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

## Update a concept
sp_list <- Easplist
summary(sp_list, "Corchorus olitorius")
sp_list <- update_concept(taxlist = sp_list, ConceptID = 155,
    Level = "subspecies")
summary(sp_list, "Corchorus olitorius")
