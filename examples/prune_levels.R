## Subset species belonging to Cyperus
Cyperus <- subset(Easplist, TaxonName == "Cyperus", slot = "taxonNames",
    keep_children = TRUE, keep_parents = TRUE)
Cyperus

## Prune not used ranks
prune_levels(Cyperus)
