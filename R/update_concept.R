# TODO:   Update information related to a concept
# 
# Author: Miguel Alvarez
################################################################################

# Generic method to update traits
setGeneric("update_trait",
		function(taxlist, ConceptID, ...)
			standardGeneric("update_trait")
)

# Method for taxlist
setMethod("update_trait", signature(taxlist="taxlist", ConceptID="numeric"),
		function(taxlist, ConceptID, ...) {
			if(any(!ConceptID %in% taxlist@taxonRelations$TaxonConceptID))
				stop("Some values of 'ConceptID' are not included as taxon concept IDs in 'taxlist'.")
			new_entries <- list(...)
			for(i in names(new_entries)[!names(new_entries) %in%
							colnames(taxlist@taxonTraits)])
				taxlist@taxonTraits[,i] <- NA
			if(any(!ConceptID %in% taxlist@taxonTraits$TaxonConceptID)) {
				df2 <-data.frame(TaxonConceptID=ConceptID[!ConceptID %in%
										taxlist@taxonTraits$TaxonConceptID],
						stringsAsFactors=FALSE)
				for(i in colnames(taxlist@taxonTraits)[
						colnames(taxlist@taxonTraits) != "TaxonConceptID"])
					df2[,i] <- NA
				taxlist@taxonTraits <- do.call(rbind, list(taxlist@taxonTraits,
								df2))
			}
			for(i in names(new_entries))
				taxlist@taxonTraits[match(ConceptID,
								taxlist@taxonTraits$TaxonConceptID),i] <-
						new_entries[[i]]
			return(taxlist)
		}
)

# Set generic method
setGeneric("update_concept",
		function(taxlist, ConceptID, ...)
			standardGeneric("update_concept")
)

# Method for taxlist
setMethod("update_concept", signature(taxlist="taxlist", ConceptID="numeric"),
		function(taxlist, ConceptID, ...) {
			if(any(!ConceptID %in% taxlist@taxonRelations$TaxonConceptID))
				stop("Some values of 'ConceptID' are not included as taxon concept IDs in 'taxlist'.")
			new_entries <- list(...)
			for(i in names(new_entries)[names(new_entries) %in%
							colnames(taxlist@taxonRelations)])
				taxlist@taxonRelations[match(ConceptID,
								taxlist@taxonRelations$TaxonConceptID),i] <-
						new_entries[[i]]
			if(any(names(new_entries) %in% colnames(taxlist@taxonTraits)))
				taxlist <- do.call(update_concept, c(list(ConceptID=ConceptID),
								new_entries[names(new_entries) %in%
												colnames(taxlist@taxonTraits)]))
			return(taxlist)
		}
)
