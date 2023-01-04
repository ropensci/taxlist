## Subset with family Ebenaceae and children
Ebenaceae <- subset(x = Easplist, subset = TaxonName == "Ebenaceae")
Ebenaceae

Ebenaceae <- get_children(Easplist, Ebenaceae)
Ebenaceae

## Get parents of Diospyros tricolor
Diostri <- subset(x = Easplist, subset = TaxonConceptID == 52403,
    slot = "relations")
Diostri

Diostri <- get_parents(Easplist, Diostri)
Diostri
