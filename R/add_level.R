# TODO:   Function adding levels in single concepts
# 
# Author: Miguel Alvarez
################################################################################

# Set generic method
setGeneric("add_level",
        function(taxlist, ConceptID, Level, ...)
            standardGeneric("add_level")
)

# Method for taxlist
setMethod("add_level", signature(taxlist="taxlist"),
        function(taxlist, ConceptID, Level, ...) {
            # Test occurrence of level
            if(!all(Level %in% levels(taxlist)))
                stop("Some values of 'Level' are invalid for this 'taxlist' object")
            # For addition of single parent to multiple children
            if(length(Level) == 1) rep(Level, length(ConceptID))
            taxlist@taxonRelations[match(ConceptID,
                            taxlist@taxonRelations$TaxonConceptID),
                    "Level"] <- Level
            return(taxlist)
        }
)

# Replacement method for convenience
setGeneric("add_level<-", function(taxlist, ConceptID, value)
            standardGeneric("add_level<-"))

# Replacement for taxlist
setReplaceMethod("add_level", signature(taxlist="taxlist"),
        function(taxlist, ConceptID, value) {
            taxlist <- add_level(taxlist, ConceptID, value)
            return(taxlist)
        }
)
