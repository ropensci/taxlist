# TODO:   Retrieves the accepted name of a concept or replace it
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("accepted_name",
        function(taxlist, ConceptID, ...)
            standardGeneric("accepted_name")
)

# Set method for taxlist
setMethod("accepted_name", signature(taxlist="taxlist"),
        function(taxlist, ConceptID, ...) {
            AcceptedName <- taxlist@taxonRelations[
                    taxlist@taxonRelations$TaxonConceptID ==
                            ConceptID,"AcceptedName"]
            return(taxlist@taxonNames[taxlist@taxonNames$TaxonUsageID ==
                                    AcceptedName,])
        }
)

# Replacement methods
setGeneric("accepted_name<-", function(taxlist, ConceptID, value)
            standardGeneric("accepted_name<-"))

# Replacement for taxlist
setReplaceMethod("accepted_name", signature(taxlist="taxlist"),
        function(taxlist, ConceptID, value) {
            # first test
            if(length(ConceptID) != length(value))
                stop("ConceptID and value should be of the same length")
            if(!all(taxlist@taxonNames[match(value,
                                    taxlist@taxonNames$TaxonUsageID),
                            "TaxonConceptID"] == ConceptID))
                stop("new value is not included in the respective taxon concept")
            # now replace
            taxlist@taxonRelations[match(ConceptID,
                            taxlist@taxonRelations$TaxonConceptID),
                    "AcceptedName"] <- value
            return(taxlist)
        }
)
