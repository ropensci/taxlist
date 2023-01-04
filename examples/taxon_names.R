## Display of slot 'taxonNames'
Euclea <- subset(x = Easplist, subset = charmatch("Euclea", TaxonName),
  slot = "names", keep_children = TRUE)
Euclea
taxon_names(Euclea)

## Insert a synonym to Diospyros scabra
summary(Easplist, "Diospyros scabra")
sp_list <- add_synonym(taxlist = Easplist, ConceptID = 51793,
  TaxonName = "Maba scabra", AuthorName = "Chiov.")
summary(sp_list, "Diospyros scabra")

## Delete a synonym of Launaea cornuta
summary(sp_list, "Launaea cornuta")
sp_list <- delete_name(sp_list, 53821)
summary(sp_list, "Launaea cornuta")

## Hypothetical correction in author name in Launaea cornuta
sp_list <- update_name(taxlist = sp_list, UsageID = 355, AuthorName = "L.")
summary(sp_list, "Launaea cornuta")
