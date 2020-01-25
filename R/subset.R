#' @name subset
#' @aliases subset,taxlist-method
#' 
#' @title Subset method for taxlist objects
#' 
#' @description 
#' Subset of \code{\linkS4class{taxlist}} objects will be done applying either
#' logical operations or pattern matchings.
#' Subsets can be referred to information contained either in the slot
#' `taxonNames`, `taxonRelations` or `taxonTraits`.
#' 
#' @param x Object of class \code{\linkS4class{taxlist}}.
#' @param subset Logical vector or logical operation to apply as subset.
#' @param slot Character value indicating the slot to be used for the subset.
#' @param keep_children Logical value applied to hierarchical structures.
#' @param keep_parents Logical value applied to hierarchical structures.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @details 
#' The argument `subset` will be applied to the slot specified in argument
#' `slot`.
#' This argument also allows partial matchings.
#' 
#' Arguments `keep_children` and `keep_parents` are applied to objects
#' including parent-child relationships.
#' When those arguments are set as `FALSE` (the default), children or parents
#' of selected taxon concepts will not be included in the subset.
#' 
#' Be aware that \code{subset} won't work properly inside of function
#' definitions.
#' 
#' @return An object of class \code{\linkS4class{taxlist}}.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}.
#' 
#' @examples 
#' Easplist <- subset(Easplist, lf_behn_2018 == "reed_plant", slot="traits")
#' summary(Easplist)
#' 
#' summary(as.factor(Easplist$lf_behn_2018))
#' 
#' @export
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
