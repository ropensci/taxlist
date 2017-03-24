# TODO:   Add a new name to a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

# Set generic method
setGeneric("add_synonym",
        function(taxlist, ConceptID, ...)
            standardGeneric("add_synonym")
)

# Method for taxlist
setMethod("add_synonym", signature(taxlist="taxlist"),
        function(taxlist, ConceptID, TaxonName, AuthorName, ...) {
            if(!ConceptID %in% taxlist@taxonRelations$TaxonConceptID)
                stop("Some values in 'ConceptID' are not included as concepts in 'taxlist'")
            # For addition of multiple synonyms to multiple concepts
            if(length(ConceptID) == 1) rep(ConceptID, length(TaxonName))
            TaxonConceptID <- ConceptID
            TaxonUsageID <- max(taxlist@taxonNames$TaxonUsageID) + 1
            TaxonUsageID <- TaxonUsageID:(TaxonUsageID + length(TaxonName) - 1)
            new_name <- list(TaxonConceptID=TaxonConceptID,
                    TaxonUsageID=TaxonUsageID, TaxonName=TaxonName,
                    AuthorName=AuthorName, ...)
            taxlist@taxonNames <- do.call(rbind,
                    list(taxlist@taxonNames,
                            new_name[match(colnames(taxlist@taxonNames),
                                            names(new_name))],
                            stringsAsFactors=FALSE))
            return(taxlist)
        }
)
