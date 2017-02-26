# TODO:   Clean loose links of a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("clean",
        function(object, ...)
            standardGeneric("clean")
)

# Method for 'taxlist' object
setMethod("clean", signature(object="taxlist"),
        function(object, ...) {
            # clean slot taxonNames
            object@taxonNames <- object@taxonNames[
                    object@taxonNames$TaxonConceptID %in%
                            object@taxonRelations$TaxonConceptID,]
            # clean slot taxonViews
            object@taxonViews <- object@taxonViews[
                    object@taxonViews$ViewID %in% object@taxonRelations$ViewID,]
            # clean slot taxonTraits
            object@taxonTraits <- object@taxonTraits[
                    object@taxonTraits$TaxonConceptID %in%
                            object@taxonRelations$TaxonConceptID,]
            return(object)
        }
)
