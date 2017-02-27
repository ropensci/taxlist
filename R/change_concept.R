# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

# Replacement methods
setGeneric("change_concept<-", function(taxlist, UsageID, value)
            standardGeneric("change_concept<-"))

# Replacement for taxlist
setReplaceMethod("change_concept", signature(taxlist="taxlist"),
        function(taxlist, UsageID, value) {
            # Test
            if(length(UsageID) != length(value))
                stop("'UsageID' and 'value' should be of the same length")
            if(any(UsageID %in% taxlist@taxonRelations$AcceptedName))
                stop("changes on concept are not allowed for accepted names")
            # now replace
            taxlist@taxonNames[match(UsageID,taxlist@taxonNames$TaxonUsageID),
                    "TaxonConceptID"] <- value
            return(taxlist)
        }
)
