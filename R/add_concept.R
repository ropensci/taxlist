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
        function(taxlist, TaxonName, Level, ...) {
            if(length(levels(taxlist)) > 0)
                orig_levels <- base::levels(taxlist@taxonRelations$Level) else
                orig_levels <- NA
            # Generating vectors
            if(nrow(taxlist@taxonRelations) == 0) TaxonConceptID <- 1 else
                TaxonConceptID <- max(taxlist@taxonRelations$TaxonConceptID) + 1
            TaxonConceptID <- TaxonConceptID:(TaxonConceptID + length(TaxonName) - 1)
            if(nrow(taxlist@taxonNames) == 0) TaxonUsageID <- 1 else
                TaxonUsageID <- max(taxlist@taxonNames$TaxonUsageID) + 1
            TaxonUsageID <- TaxonUsageID:(TaxonUsageID + length(TaxonName) - 1)
            if(!missing(Level)) {
                if(length(Level) == 1) Level <- rep(Level, length(TaxonName))
            } else Level <- rep(NA, length(TaxonName))
            new_concept <- list(TaxonConceptID=TaxonConceptID,
                    TaxonUsageID=TaxonUsageID, TaxonName=TaxonName, Level=Level,
                    ...)
            new_concept[["AcceptedName"]] <- TaxonConceptID
            # Add missing variables
            for(i in colnames(taxlist@taxonNames)[
                    !colnames(taxlist@taxonNames) %in% names(new_concept)]) {
                new_concept[[i]] <- rep(NA, length(TaxonName))
            }
            for(i in colnames(taxlist@taxonRelations)[
                    !colnames(taxlist@taxonRelations) %in%
                            names(new_concept)]) {
                new_concept[[i]] <- rep(NA, length(TaxonName))
            }
            # Merge old and new information
            taxlist@taxonRelations <- do.call(rbind,
                    list(taxlist@taxonRelations,
                            new_concept[match(colnames(taxlist@taxonRelations),
                                            names(new_concept))]))
            taxlist@taxonNames <- do.call(rbind,
                    list(taxlist@taxonNames,
                            new_concept[match(colnames(taxlist@taxonNames),
                                            names(new_concept))],
                            stringsAsFactors=FALSE))
            if(!all(is.na(orig_levels)))
                taxlist@taxonRelations$Level <- factor(
                        paste(taxlist@taxonRelations$Level), levels=orig_levels)
            return(taxlist)
        }
)
