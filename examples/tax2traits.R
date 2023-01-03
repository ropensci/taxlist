## Family Acanthaceae with children
Acanthaceae <- subset(x = Easplist, subset = TaxonName == "Acanthaceae",
  slot = "names", keep_children = TRUE)
summary(Acanthaceae)

## Insert taxonomy to taxon traits
Acanthaceae <- tax2traits(Acanthaceae, get_names = TRUE)
head(taxon_traits(Acanthaceae))
