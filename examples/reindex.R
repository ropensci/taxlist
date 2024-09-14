library(taxlist)

# Copy taxonomic list
sp_list <- Easplist
summary(sp_list, "papyrus")

# Re-index taxon concepts
sp_list <- reindex(sp_list, new = id_generator(nrow(sp_list@taxonRelations),
    mode = "character"))

# Re-index taxon usage names
sp_list <- reindex(sp_list, new = id_generator(nrow(sp_list@taxonNames),
    mode = "character"), idx = "TaxonUsageID")

# Re-index taxon views
sp_list <- reindex(sp_list, new = id_generator(nrow(sp_list@taxonViews),
    mode = "character"), idx = "ViewID")

# Check result
validObject(sp_list)
summary(sp_list, "papyrus")
