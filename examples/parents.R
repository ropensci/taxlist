# Random selection of 5 taxa
IDs <- sample(Easplist@taxonRelations$TaxonConceptID, 5)

# Print names and names of parents
print_name(Easplist, IDs)
print_name(Easplist, parents(Easplist, "genus", IDs))
print_name(Easplist, parents(Easplist, "family", IDs))
