# TODO:   Add a new name to a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

# Set generic method
setGeneric("add_concept",
        function(taxlist, TaxonName, ...)
            standardGeneric("add_concept")
)

# Method for taxlist and character
setMethod("add_concept", signature(taxlist="taxlist", TaxonName="character"),
        function(taxlist, TaxonName, Level, ...) {
            if(length(levels(taxlist)) > 0)
                orig_levels <- base::levels(taxlist@taxonRelations$Level) else
                orig_levels <- NA
            # Generating vectors
            if(nrow(taxlist@taxonRelations) == 0) TaxonConceptID <- 1 else
                TaxonConceptID <- max(taxlist@taxonRelations$TaxonConceptID) + 1
            TaxonConceptID <- TaxonConceptID:(TaxonConceptID +
						length(TaxonName) - 1)
            if(nrow(taxlist@taxonNames) == 0) TaxonUsageID <- 1 else
                TaxonUsageID <- max(taxlist@taxonNames$TaxonUsageID) + 1
            TaxonUsageID <- TaxonUsageID:(TaxonUsageID + length(TaxonName) - 1)
            if(!missing(Level)) {
                if(length(Level) == 1) Level <- rep(Level, length(TaxonName))
            } else Level <- rep(NA, length(TaxonName))
            new_concept <- list(TaxonConceptID=TaxonConceptID,
                    TaxonUsageID=TaxonUsageID, TaxonName=TaxonName, Level=Level,
                    ...)
            new_concept[["AcceptedName"]] <- TaxonUsageID
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

# Method for taxlist and taxlist
setMethod("add_concept", signature(taxlist="taxlist", TaxonName="taxlist"),
		function(taxlist, TaxonName, insert_view, ...) {
			# First check
			if(any(with(TaxonName@taxonNames, paste(TaxonName,
									AuthorName)) %in%
					with(taxlist@taxonNames, paste(TaxonName, AuthorName))))
				stop("Shared combinations are not allowed.")
			# Change taxon views, if necessary
			if(!missing(insert_view)) {
				if(insert_view)
					old_view <- TaxonName@taxonViews$ViewID
				TaxonName@taxonViews$ViewID <- new_view <-
						max(taxlist@taxonViews$ViewID) + seq_along(old_view)
				TaxonName@taxonRelations$ViewID <-
						with(TaxonName@taxonRelations, replace_x(ViewID,
										old_view, new_view))
				taxlist@taxonViews <- insert_rows(taxlist@taxonViews,
						TaxonName@taxonViews)
			}
			# Change names
			old_names <- TaxonName@taxonNames$TaxonUsageID
			new_names <- max(taxlist@taxonNames$TaxonUsageID)
			new_names <- (new_names + 1):(new_names + length(old_names))
			TaxonName@taxonNames$TaxonUsageID <- new_names
			TaxonName@taxonRelations$AcceptedName <- new_names[
					match(TaxonName@taxonRelations$AcceptedName, old_names)]
			TaxonName@taxonRelations$Basionym <- new_names[
					match(TaxonName@taxonRelations$Basionym, old_names)]
			# Change concepts
			old_concepts <- TaxonName@taxonRelations$TaxonConceptID
			new_concepts <- max(taxlist@taxonRelations$TaxonConceptID)
			new_concepts <- (new_concepts + 1):(new_concepts +
						length(old_concepts))
			TaxonName@taxonRelations$TaxonConceptID <- new_concepts
			TaxonName@taxonRelations$Parent <- new_concepts[
					match(TaxonName@taxonRelations$Parent, old_concepts)]
			TaxonName@taxonNames$TaxonConceptID <- new_concepts[
					match(TaxonName@taxonNames$TaxonConceptID, old_concepts)]
			if(nrow(TaxonName@taxonTraits) > 0)
				TaxonName@taxonTraits$TaxonConceptID <- new_concepts[
						match(TaxonName@taxonTraits$TaxonConceptID,
								old_concepts)]
			# Add slot taxon Names to 'taxlist'
			taxlist@taxonNames <- two2one_df(taxlist@taxonNames,
					TaxonName@taxonNames)
			taxlist@taxonRelations <- two2one_df(taxlist@taxonRelations,
					TaxonName@taxonRelations)
			# Add taxonTraits
			if(nrow(taxlist@taxonTraits) == 0 & nrow(TaxonName@taxonTraits) > 0)
				taxlist@taxonTraits <- TaxonName@taxonTraits
			if(nrow(taxlist@taxonTraits) > 0 & nrow(TaxonName@taxonTraits) > 0)
				taxlist@taxonTraits <- two2one_df(taxlist@taxonTraits,
					TaxonName@taxonTraits)
			return(taxlist)
		}
)
