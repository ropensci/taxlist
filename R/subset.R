# TODO:   subset methods for taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# subset method for taxlist objects
setMethod("subset", signature(x="taxlist"),
        function(x, subset, slot="names", keep_children=FALSE,
                keep_parents=FALSE, ...) {
            slot <- grep(slot[1], slotNames(x), ignore.case=TRUE)
            if(length(slot) == 0)
                stop("Invalid value for argument 'slot'")
            slot <- slotNames(x)[slot]
            subset <- substitute(subset)
            subset <- eval(subset, slot(x, slot), parent.frame())
            if(slot %in% c("taxonNames","taxonRelations","taxonTraits"))
                subset <- unique(slot(x, slot)[subset,"TaxonConceptID"])
            else if(slot == "taxonViews") {
                subset <- unique(slot(x, slot)[subset,"ViewID"])
                subset <- x@taxonRelations[x@taxonRelations$ViewID %in% subset,
                        "TaxonConceptID"]
            }
            z <- x
            z@taxonRelations <- x@taxonRelations[
                    x@taxonRelations$TaxonConceptID %in% subset,]
            z <- clean(z)
            if(keep_children)
                z <- get_children(x, z)
            if(keep_parents)
                z <- get_parents(x, z)
            return(z)
        }
)
