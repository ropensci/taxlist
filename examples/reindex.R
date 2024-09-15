## Copy taxonomic list
sp_list <- Easplist
summary(sp_list, "papyrus")

## Re-index taxon concepts
reindex(sp_list) <- id_generator(nrow(sp_list@taxonRelations),
    mode = "character")

## Re-index taxon usage names
reindex(sp_list, idx = "TaxonUsageID") <- id_generator(nrow(sp_list@taxonNames),
    mode = "character")

## Re-index taxon views
reindex(sp_list, idx = "ViewID") <- id_generator(nrow(sp_list@taxonViews),
    mode = "character")

## Check result
validObject(sp_list)
summary(sp_list, "papyrus")
