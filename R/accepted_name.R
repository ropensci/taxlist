# TODO:   Retrieves the accepted name of a concept or replace it
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("accepted_name",
        function(taxlist, ConceptID, ...)
            standardGeneric("accepted_name")
)

# Provide accepted names in a data frame
setMethod("accepted_name", signature(taxlist="taxlist", ConceptID="numeric"),
        function(taxlist, ConceptID, ...) {
            AcceptedName <- taxlist@taxonRelations[
                    taxlist@taxonRelations$TaxonConceptID %in%
                            ConceptID,c("TaxonConceptID","AcceptedName")]
            for(i in c("TaxonName","AuthorName"))
                AcceptedName[,i] <- taxlist@taxonNames[
                        match(AcceptedName$AcceptedName,
                                taxlist@taxonNames$TaxonUsageID),i]
            colnames(AcceptedName)[2] <- "TaxonUsageID"
            return(AcceptedName)
        }
)

# Method for the whole object
setMethod("accepted_name", signature(taxlist="taxlist", ConceptID="missing"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- taxlist@taxonRelations$TaxonConceptID
            return(accepted_name(taxlist, ConceptID, ...))
        }
)

# Replacement methods
setGeneric("accepted_name<-", function(taxlist, ConceptID, value)
            standardGeneric("accepted_name<-"))

# Replacement for taxlist
setReplaceMethod("accepted_name", signature(taxlist="taxlist",
                ConceptID="numeric", value="numeric"),
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
