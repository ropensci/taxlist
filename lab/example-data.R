# TODO:   Data sets with different properties
# 
# Author: Miguel Alvarez
################################################################################

library(taxlist)
library(vegStore)
library(vegan)

# Load World Flora Online and subset
wfo <- download("wfo")
junc <- subset(wfo, TaxonName == "Juncaceae", slot = "taxonNames",
    keep_children = TRUE)
junc

saveRDS(junc, "lab/juncaceae.rda")
## junc <- readRDS("lab/juncaceae.rda")

# Do character values
names_id <- data.frame(
    old = junc@taxonNames$TaxonUsageID,
    new = id_generator(len = nrow(junc@taxonNames), mode = "character",
        nchar = 6))

# Replace
junc@taxonNames$TaxonUsageID <- replace_x(junc@taxonNames$TaxonUsageID,
    old = names_id$old, new = names_id$new)
junc@taxonRelations$AcceptedName <- replace_x(junc@taxonRelations$AcceptedName,
    old = names_id$old, new = names_id$new)

junc

# IDs for taxon concepts
concepts_id <- junc@taxonRelations[ , c("TaxonConceptID", "AcceptedName")]
concepts_id$new <- make.cepnames(junc@taxonNames$TaxonName[
        match(concepts_id$AcceptedName, junc@taxonNames$TaxonUsageID)])

junc@taxonRelations$TaxonConceptID <- replace_x(
    junc@taxonRelations$TaxonConceptID,
    old = concepts_id$TaxonConceptID,
    new = concepts_id$new)

junc@taxonRelations$Parent <- replace_x(
    junc@taxonRelations$Parent,
    old = concepts_id$TaxonConceptID,
    new = concepts_id$new)

junc@taxonTraits$TaxonConceptID <- replace_x(
    junc@taxonTraits$TaxonConceptID,
    old = concepts_id$TaxonConceptID,
    new = concepts_id$new)

junc@taxonNames$TaxonConceptID <- replace_x(
    junc@taxonNames$TaxonConceptID,
    old = concepts_id$TaxonConceptID,
    new = concepts_id$new)

# No more working
junc
summary(junc, "all")

# Change views
views_id <- data.frame(
    old = junc@taxonViews$ViewID,
    new = id_generator(len = nrow(junc@taxonViews), mode = "character",
        nchar = 4))

junc@taxonViews$ViewID <- replace_x(junc@taxonViews$ViewID,
    old = views_id$old, new = views_id$new)
junc@taxonRelations$ViewID <- replace_x(junc@taxonRelations$ViewID,
    old = views_id$old, new = views_id$new)

junc
summary(junc, "all")

saveRDS(junc, "lab/juncaceae-char.rda")
