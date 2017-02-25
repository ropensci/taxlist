# TODO:   Methods dealing with slot taxonTraits
# 
# Author: Miguel Alvarez
################################################################################

# Generic method
setGeneric("taxon_traits",
        function(taxlist, ...)
            standardGeneric("taxon_traits")
)

# Set method for taxlist
setMethod("taxon_traits", signature(taxlist="taxlist"),
        function(taxlist, ...) taxlist@taxonTraits
)

# Generic for replacement method
setGeneric("taxon_traits<-", function(taxlist, value)
            standardGeneric("taxon_traits<-"))

# Definition of method
setReplaceMethod("taxon_traits", signature(taxlist="taxlist",
                value="data.frame"),
        function(taxlist, value) {
            if(!"TaxonConceptID" %in% colnames(value))
                stop("'TaxonConceptID' is a mandatory field in 'value'")
            if(class(value$TaxonConceptID) != "integer")
                value$TaxonConceptID <- as.integer(value$TaxonConceptID)
            if(any(duplicated(value$TaxonConceptID))) {
                warning("duplicated concepts will be deleted from 'value'")
                value <- value[unique(value$TaxonConceptID),]
            }
            taxlist@taxonTraits <- value[value$TaxonConceptID %in%
                            taxlist@taxonRelations$TaxonConceptID,]
            return(taxlist)
        }
)
