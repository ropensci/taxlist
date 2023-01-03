## Displaying taxon traits
head(taxon_traits(Easplist))

## Updating traits for Launaea cornuta
summary(Easplist, "Launaea cornuta")
accepted_name(taxlist = Easplist, ConceptID = 355, show_traits = TRUE)

sp_list <- update_trait(taxlist = Easplist, ConceptID = 355,
    life_form = "annual")
accepted_name(taxlist = sp_list, ConceptID = 355, show_traits = TRUE)
