count_children <- function(tax_list, taxon_select, level_select) {
    require(taxlist)

    # Do subset without subset()
    tax_filter <- tax_list
    tax_filter@taxonNames <- tax_list@taxonNames[
        tax_list@taxonNames$TaxonName == taxon_select,
    ]
    tax_filter <- taxlist::clean(tax_filter)
    tax_filter <- get_children(tax_list, tax_filter)

    # Now count children
    return(taxlist::count_taxa(tax_filter, level = level_select))
}

# Test the function
count_children(Easplist, "Cyperus", "species")

# Cross-check
subset(Easplist, TaxonName == "Cyperus",
    slot = "taxonNames",
    keep_children = TRUE, keep_parents = TRUE
)
