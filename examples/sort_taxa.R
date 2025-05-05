## Subset with Boraginaceae
tax <- subset(Easplist, TaxonName %in% c("Boraginaceae"),
    keep_children = TRUE, keep_parents = TRUE
)
indented_list(tax)

## Sorting names alphabetically
tax <- sort_taxa(tax)
indented_list(tax)

## Setting some names on top of the sorting
tax <- sort_taxa(tax, priority = c("Euploca", "Myosotis", "Cordia monoica"))
indented_list(tax)
