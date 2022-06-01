# TODO:   Generates example for function load last
#
# Author: Miguel Alvarez
################################################################################

Podocarpus <- subset(Easplist, TaxonName == "Podocarpus",
  keep_children = TRUE,
  keep_parents = TRUE
)
backup_object(Podocarpus, file = "inst/extdata/Podocarpus")
