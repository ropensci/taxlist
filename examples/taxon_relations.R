## Subset for the genus Euclea and display of slot 'taxonNames'
Euclea <- subset(x = Easplist, subset = charmatch("Euclea", TaxonName),
  slot = "names", keep_children = TRUE)
Euclea
taxon_relations(Euclea)
