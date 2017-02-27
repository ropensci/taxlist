# TODO:   Function adding parent in single concepts
# 
# Author: Miguel Alvarez
################################################################################

# Set generic method
setGeneric("add_parent",
        function(taxlist, ConceptID, Parent, ...)
            standardGeneric("add_parent")
)

# Method for taxlist
setMethod("add_parent", signature(taxlist="taxlist"),
        function(taxlist, ConceptID, Parent, ...) {
            if(any(Parent %in% taxlist@taxonRelations$TaxonConceptID))
                stop("Some new parent values are not included as concept IDs in 'taxlist'")
            # For addition of single parent to multiple children
            if(length(Parent) == 1) rep(Parent, length(ConceptID))
            taxlist@taxonRelations[match(ConceptID,
                            taxlist@taxonRelations$TaxonConceptID),
                    "Parent"] <- Parent
            return(taxlist)
        }
)

# Replacement method for convenience
setGeneric("add_parent<-", function(taxlist, ConceptID, value)
            standardGeneric("add_parent<-"))

# Replacement for taxlist
setReplaceMethod("add_parent", signature(taxlist="taxlist"),
        function(taxlist, ConceptID, value) {
            taxlist <- add_parent(taxlist, ConceptID, value)
            return(taxlist)
        }
)
