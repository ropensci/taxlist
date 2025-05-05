# TODO:   New function matching names
# 
# Author: Miguel Alvarez
################################################################################

library(taxlist)
## library(stringdist)

## Names to be compared
species <- c("Cperus papyrus", "Typha australis", "Luke skywalker")
my_names <- c("Cyperus paper", "TIE fighter")

## Comparing character vectors
(comp_tab <- match_names(my_names, species, best = 2))
(comp_tab <- match_names(my_names, species, cutlevel = 0.8))

## Comparing a vector with itself
species2 <- c("Apium graveolens", "Allium cepa",
    "Asparagus officinalis", "Allium sativum",
    "Allium cepo")
(comp_tab <- match_names(species2))
(comp_tab <- match_names(species2, cutlevel = 0.8))
(comp_tab <- match_names(species2, cutlevel = 0.8, nomatch = FALSE))

# Comparing with taxlist object
(comp_tab <- match_names(species2, Easplist, cutlevel = 0.85))
(comp_tab <- match_names(species2, Easplist, cutlevel = 0.85,
          show_concepts = TRUE))

# Including author name
(comp_tab <- match_names(paste(c("Prosopis juliflora", "Opuntia vulgaris"),
              "L."), Easplist, include_author = TRUE))

