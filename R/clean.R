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
            if(nrow(object@taxonViews) > 0)
                object@taxonViews <- object@taxonViews[
                        object@taxonViews$ViewID %in%
                                object@taxonRelations$ViewID,]
            # clean slot taxonTraits
            if(nrow(object@taxonTraits) > 0)
                object@taxonTraits <- object@taxonTraits[
                        object@taxonTraits$TaxonConceptID %in%
                                object@taxonRelations$TaxonConceptID,]
            # clean parents (deleted parents)
            object@taxonRelations$Parent[!object@taxonRelations$Parent %in%
                            object@taxonRelations$TaxonConceptID] <- NA
			# clean classes of key fields
			object@taxonNames$TaxonConceptID <- as.integer(object@taxonNames$TaxonConceptID)
			object@taxonNames$TaxonUsageID <- as.integer(object@taxonNames$TaxonUsageID)
			object@taxonNames$TaxonName <- paste(object@taxonNames$TaxonName)
			object@taxonNames$AuthorName <- paste(object@taxonNames$AuthorName)
			object@taxonRelations$TaxonConceptID <- as.integer(object@taxonRelations$TaxonConceptID)
			object@taxonRelations$AcceptedName <- as.integer(object@taxonRelations$AcceptedName)
			object@taxonRelations$Basionym <- as.integer(object@taxonRelations$Basionym)
			object@taxonRelations$Parent <- as.integer(object@taxonRelations$Parent)
			object@taxonRelations$ViewID <- as.integer(object@taxonRelations$ViewID)
			object@taxonTraits$TaxonConceptID <- as.integer(object@taxonTraits$TaxonConceptID)
			object@taxonViews$ViewID <- as.integer(object@taxonViews$ViewID)
			return(object)
        }
)
