# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(taxlist)

Spp <- c("Cyperus papirus", "Thypha dominguez", "Eichhornia crassipes")

# Unsorted output
match_names(Spp, Easplist)

# Sorting by names
match_names(Spp, Easplist, sort_by = "TaxonName")

# Decreasing sorting by similarity
match_names(Spp, Easplist, sort_by = "similarity", order_args = list(decreasing = TRUE))
