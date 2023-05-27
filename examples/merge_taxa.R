## Merge Cyperus papyrus and Cyperus dives
summary(Easplist, c(206, 197))

merged_cyperus <- merge_taxa(object = Easplist, concepts = c(206, 197),
    print_output = TRUE)

## Subset with Kyllinga species
ky <- subset(Easplist, TaxonName == "Kyllinga", keep_children = TRUE,
    keep_parents = TRUE)
ky
indented_list(ky)

## Merge to species and family
merge_taxa(ky, level = c("species", "family"))

## Merge to variety and genus
merge_taxa(ky, level = c("variety", "genus"))
