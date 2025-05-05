# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(taxlist)

IDs <- sample(Easplist@taxonRelations$TaxonConceptID, 5)

print_name(Easplist, IDs)
print_name(Easplist, parents(Easplist, "genus", IDs))
print_name(Easplist, parents(Easplist, "family", IDs))
