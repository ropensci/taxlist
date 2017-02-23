# TODO:   Add a new name to a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

# Set generic method
setGeneric("add_concept",
        function(taxlist, ...)
            standardGeneric("add_concept")
)

# Method for taxlist
setMethod("add_concept", signature(taxlist="taxlist"),
        function(taxlist, TaxonName, AuthorName, Parent, Level, ViewID, ...) {
            # Generating vectors
            if(nrow(taxlist@taxonRelations) == 0) TaxonConceptID <- 1 else
                TaxonConceptID <- max(taxlist@taxonRelations$TaxonConceptID)
            TaxonConceptID <- TaxonConceptID:(TaxonConceptID + length(TaxonName) - 1)
            if(nrow(taxlist@taxonNames) == 0) TaxonUsageID <- 1 else
                TaxonUsageID <- max(taxlist@taxonNames$TaxonUsageID)
            TaxonUsageID <- TaxonUsageID:(TaxonUsageID + length(TaxonName) - 1)
            if(missing(AuthorName)) AuthorName <- rep(NA, length(TaxonName))
            if(missing(Parent)) Parent <- rep(NA, length(TaxonName))
            if(missing(Level)) Level <- rep(NA, length(TaxonName))
            if(missing(ViewID)) ViewID <- rep(NA, length(TaxonName))
            new_concept <- nlist(TaxonConceptID, TaxonUsageID, TaxonName,
                    AuthorName, Parent, Level, ViewID, ...)
            # Add missing variables
            for(i in colnames(taxlist@taxonNames)[
                    !colnames(taxlist@taxonNames) %in% names(new_concept)]) {
                new_concept[[i]] <- rep(NA, length(TaxonName))
            }
            taxlist@taxonRelations <- do.call(rbind,
                    list(taxlist@taxonRelations,
                            new_concept[names(new_concept) %in%
                                            colnames(taxlist@taxonRelations)]))
            taxlist@taxonNames <- do.call(rbind,
                    list(taxlist@taxonNames,
                            new_concept[names(new_concept) %in%
                                            colnames(taxlist@taxonNames)],
                            stringsAsFactors=FALSE))
            return(taxlist)
        }
)
