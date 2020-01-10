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
setMethod("get_children", signature(taxlist="taxlist", ConceptID="numeric"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- list(ConceptID)
            repeat {
                if(!any(ConceptID[[length(ConceptID)]] %in%
                                taxlist@taxonRelations$Parent)) break
                ConceptID[[length(ConceptID) + 1]] <- taxlist@taxonRelations[
                        taxlist@taxonRelations$Parent %in%
                                ConceptID[[length(ConceptID)]],
                        "TaxonConceptID"]
            }
            ConceptID <- do.call(c, ConceptID)
            taxlist@taxonRelations <- taxlist@taxonRelations[
                    taxlist@taxonRelations$TaxonConceptID %in% ConceptID,]
            return(clean(taxlist))
        }
)

# Set method for taxlist,taxlist method (e.g using subsets)
setMethod("get_children", signature(taxlist="taxlist", ConceptID="taxlist"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- ConceptID@taxonRelations$TaxonConceptID
            return(get_children(taxlist, ConceptID))
        }
)

# Generic function to get parents
setGeneric("get_parents",
        function(taxlist, ConceptID, ...)
            standardGeneric("get_parents")
)

# Set method for taxlist
setMethod("get_parents", signature(taxlist="taxlist", ConceptID="numeric"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- list(ConceptID)
            repeat {
                if(all(is.na(taxlist@taxonRelations[
                                     taxlist@taxonRelations$TaxonConceptID %in%
                                                ConceptID[[length(ConceptID)]],
                                        "Parent"]))) break
                ConceptID[[length(ConceptID) + 1]] <- taxlist@taxonRelations[
                        taxlist@taxonRelations$TaxonConceptID %in%
                                ConceptID[[length(ConceptID)]], "Parent"]
            }
            ConceptID <- do.call(c, ConceptID)
            ConceptID <- na.omit(ConceptID)
            taxlist@taxonRelations <- taxlist@taxonRelations[
                    taxlist@taxonRelations$TaxonConceptID %in% ConceptID,]
            return(clean(taxlist))
        }
)

# Set method for taxlist,taxlist method (e.g using subsets)
setMethod("get_parents", signature(taxlist="taxlist", ConceptID="taxlist"),
        function(taxlist, ConceptID, ...) {
            ConceptID <- ConceptID@taxonRelations$TaxonConceptID
            return(get_parents(taxlist, ConceptID))
        }
)
