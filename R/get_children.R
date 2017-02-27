# TODO:   Methods to get children or parents of a concept
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("get_children",
        function(taxlist, ConceptID, ...)
            standardGeneric("get_children")
)

# Set method for taxlist
setMethod("get_children", signature(taxlist="taxlist"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- list(ConceptID)
            repeat {
                if(!any(taxlist@taxonRelations$Parent %in%
                                ConceptID[[length(ConceptID)]])) break
                ConceptID[[length(ConceptID) + 1]] <- taxlist@taxonRelations[
                        taxlist@taxonRelations$Parent %in% ConceptID,
                        "TaxonConceptID"]
            }
            ConceptID <- do.call(c, ConceptID)
            return(subset(taxlist, TaxonConceptID %in% ConceptID,
                            slot="taxonRelations"))
        }
)

# Generic function to get parents
setGeneric("get_parents",
        function(taxlist, ConceptID, ...)
            standardGeneric("get_parents")
)

# Set method for taxlist
setMethod("get_parents", signature(taxlist="taxlist"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- list(ConceptID)
            repeat {
                if(all(is.na(taxlist@taxonRelations[
                                        taxlist@taxonRelations$TaxonConceptID %in%
                                                ConceptID,"Parent"]))) break
                ConceptID[[length(ConceptID) + 1]] <- taxlist@taxonRelations[
                        taxlist@taxonRelations$TaxonConceptID %in% ConceptID,
                        "Parent"]
            }
            ConceptID <- do.call(c, ConceptID)
            ConceptID <- na.omit(ConceptID)
            return(subset(taxlist, TaxonConceptID %in% ConceptID,
                            slot="taxonRelations"))
        }
)
