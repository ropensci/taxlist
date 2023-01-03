# A list of variety names
sp_list <- subset(x = Easplist, subset = Level == "variety", slot = "relations")
sp_list <- accepted_name(sp_list)[c(1:10), "TaxonName"]

# split name
dissect_name(sp_list)

# re-paste the two first words (species name)
dissect_name(sp_list, repaste = c(1:2))
