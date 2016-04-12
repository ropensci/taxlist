# TODO:   Merge sub-specific taxa into species
# 
# Author: Miguel Alvarez
################################################################################

# Generic function -------------------------------------------------------------
setGeneric("merge_species",
        function(taxlist, ...)
            standardGeneric("merge_species")
)

# Method for taxlist -----------------------------------------------------------
setMethod("merge_species", signature(taxlist="taxlist"),
        function(taxlist, ...) {
            # Detect taxa sharing the two first names
            Bin <- taxlist@taxonNames[
                    paste(taxlist@taxonRelations$AcceptedName), "TaxonName",
                    drop=TRUE]
            Bin <- sapply(strsplit(Bin, " ", fixed=TRUE), "[", c(1:2))
            Bin <- split(paste(taxlist@taxonRelations$AcceptedName),
                    paste(Bin[1,], Bin[2,]))
            Bin <- Bin[sapply(Bin, length) > 1]
            for(i in names(Bin)) {
                Bin[[i]] <- cbind(old=as.integer(Bin[[i]]),
                        new=as.integer(Bin[[i]][1]))
            }
            Bin <- do.call(rbind, Bin)
            # Re-arrange object
            taxlist@taxonNames$TaxonConceptID_old <- NewConcepts <-
                    taxlist@taxonNames$TaxonConceptID
            for(i in 1:nrow(Bin)) {
                NewConcepts[NewConcepts == Bin[i,"old"]] <- Bin[i,"new"]
            }
            taxlist@taxonNames$TaxonConceptID <- NewConcepts
            taxlist@taxonRelations <- taxlist@taxonRelations[
                    taxlist@taxonRelations$AcceptedName %in% NewConcepts,]
            return(taxlist)
        }
)
