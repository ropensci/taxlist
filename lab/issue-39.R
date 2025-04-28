# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

## library(taxlist)

## save(list = ls(), file = "lab/data-39.rda")
## save(object, revision, key, add, update, taxlist, file = "lab/data-39.rda")
load(file = "lab/data-39.rda")

library(biblio)

taxlist@taxonTraits <- update_data(
    object = taxlist@taxonTraits,
    revision = taxonTraits, key = "TaxonConceptID", add = TRUE,
    update = TRUE
)


object = Papyrus
get_names = alphabetical = FALSE

# Update trait
taxlist = object
taxonTraits = TAX

object = taxlist@taxonTraits
revision = taxonTraits
key = "TaxonConceptID"
add = TRUE
update = TRUE



, filter, keep_children = TRUE, keep_parents = TRUE,
rankless_as, indent = " ", lead_br = "", print = TRUE,
author = TRUE, level = FALSE, synonyms = FALSE,
syn_encl = c("= ", ""), secundum, alphabetical = FALSE,




