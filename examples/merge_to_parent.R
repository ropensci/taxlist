## Subset with Kyllinga species
ky <- subset(Easplist, TaxonName == "Kyllinga", keep_children = TRUE,
    keep_parents = TRUE)
ky
indented_list(ky)

## Merge two species with the genus
summary(ky, c(346, 50400))
summary(ky, "Kyllinga", exact = TRUE)
ky <- merge_to_parent(ky, c(346, 50400))

summary(ky, "Kyllinga", exact = TRUE)
