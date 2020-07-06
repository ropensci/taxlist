#' @name taxon_relations
#' 
#' @title Retrieve or replace slot taxonRelations in taxlist objects
#' 
#' @description 
#' Retrieve the content of slot `taxonRelations` from a
#' [taxlist-class] object or replace it by a new data frame.
#' 
#' @param taxlist A [taxlist-class] object.
#' @param value A `data.frame` object to be set as slot `taxonRelations`.
#' @param TaxonName Character vector with the accepted name for the new taxon
#'     concepts.
#' @param Level Character vector indicating the level of the concept in the
#'     list.
#' @param insert_view A numeric (integer) vector, indicating the views to be
#'     inserted in `taxlist` or the value `TRUE` (see details).
#' @param ConceptID Concept IDs to be updated.
#' @param ... Further arguments passed among methods.
#' 
#' @details 
#' The replacement method `taxon_relations<-` should be only used when
#' constructing [taxlist-class] objects from an empty one
#' (prototype).
#' 
#' New concepts should be first added to a [taxlist-class] object
#' using their respective accepted names.
#' Synonyms can be further provided using the function [add_synonym()].
#' 
#' Additional named vectors can be provided to be included in slot `taxonNames`,
#' in the cases where those variables already exist, otherwise they will be
#' ignored.
#' 
#' It is recommended also to provide a concept view as `ViewID` (see
#' [taxon_views()]).
#' For adding a new view, use [add_view()].
#' 
#' @return 
#' An object of class [taxlist-class] with added names and
#' concepts.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [taxlist-class]
#' 
#' @examples 
#' ## Subset for the genus Euclea and display of slot 'taxonNames'
#' Euclea <- subset(x=Easplist, subset=charmatch("Euclea", TaxonName),
#'     slot="names")
#' Euclea <- get_children(Easplist, Euclea)
#' 
#' Euclea
#' taxon_relations(Euclea)
#' 
#' ## Subset with family Ebenaceae and children
#' Ebenaceae <- subset(Easplist, charmatch("Ebenaceae", TaxonName))
#' Ebenaceae <- get_children(Easplist, Ebenaceae)
#' 
#' Ebenaceae
#' summary(object=Ebenaceae, ConceptID="all", maxsum=100)
#' 
#' ## Adding a new concept
#' Ebenaceae <- add_concept(taxlist=Ebenaceae, TaxonName="Euclea acutifolia",
#'     AuthorName="E. Mey. ex A. DC.", Level="species", Parent=55707, ViewID=1)
#' 
#' ## A summary again  
#' Ebenaceae
#' summary(Ebenaceae, "all", maxsum=100)
#' 
#' ## Display two Typha species
#' summary(Easplist, c("Typha domingensis","Typha latifolia"))
#' 
#' ## Update a concept
#' summary(Easplist, "Corchorus olitorius")
#' Easplist <- update_concept(taxlist=Easplist, ConceptID=155,
#'     Level="subspecies")
#' summary(Easplist, "Corchorus olitorius")
#' 
#' @rdname taxon_relations
#' 
#' @exportMethod taxon_relations
#' 
setGeneric("taxon_relations",
        function(taxlist, ...)
            standardGeneric("taxon_relations")
)

#' @rdname taxon_relations
#' 
#' @aliases taxon_relations,taxlist-method
#' 
setMethod("taxon_relations", signature(taxlist="taxlist"),
        function(taxlist, ...) taxlist@taxonRelations
)

#' @rdname taxon_relations
#' 
#' @aliases taxon_relations<-
#' 
#' @exportMethod taxon_relations<-
#' 
setGeneric("taxon_relations<-", function(taxlist, value)
            standardGeneric("taxon_relations<-"))

#' @rdname taxon_relations
#' 
#' @aliases taxon_relations<-,taxlist,data.frame-method
#' 
setReplaceMethod("taxon_relations", signature(taxlist="taxlist",
                value="data.frame"), function(taxlist, value) {
            taxlist@taxonRelations <- value
            return(taxlist)
        }
)

#' @rdname taxon_relations
#' 
#' @aliases add_concept
#' 
#' @exportMethod add_concept
#' 
setGeneric("add_concept",
		function(taxlist, TaxonName, ...)
			standardGeneric("add_concept")
)

#' @rdname taxon_relations
#' 
#' @aliases add_concept,taxlist,character-method
#' 
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

#' @rdname taxon_relations
#' 
#' @aliases add_concept,taxlist,taxlist-method
#' 
setMethod("add_concept", signature(taxlist="taxlist", TaxonName="taxlist"),
		function(taxlist, TaxonName, insert_view, ...) {
			# First check
			## if(any(with(TaxonName@taxonNames, paste(TaxonName,
			##                                 AuthorName)) %in%
			##                 with(taxlist@taxonNames, paste(TaxonName, AuthorName))))
			if(any(paste(TaxonName@taxonNames$TaxonName,
									TaxonName@taxonNames$AuthorName) %in%
							paste(taxlist@taxonNames$TaxonName,
									taxlist@taxonNames$AuthorName)))
				stop("Shared combinations are not allowed.")
			# Change taxon views, if necessary
			if(!missing(insert_view)) {
				if(insert_view)
					old_view <- TaxonName@taxonViews$ViewID
				TaxonName@taxonViews$ViewID <- new_view <-
						max(taxlist@taxonViews$ViewID) + seq_along(old_view)
				## TaxonName@taxonRelations$ViewID <-
				##         with(TaxonName@taxonRelations, replace_x(ViewID,
				##                         old_view, new_view))
				TaxonName@taxonRelations$ViewID <- replace_x(
						TaxonName@taxonRelations$ViewID, old_view, new_view)
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

#' @rdname taxon_relations
#' 
#' @aliases update_concept
#' 
#' @exportMethod update_concept
#' 
setGeneric("update_concept",
		function(taxlist, ConceptID, ...)
			standardGeneric("update_concept")
)

#' @rdname taxon_relations
#' 
#' @aliases update_concept,taxlist,numeric-method
#' 
setMethod("update_concept", signature(taxlist="taxlist", ConceptID="numeric"),
		function(taxlist, ConceptID, ...) {
			if(any(!ConceptID %in% taxlist@taxonRelations$TaxonConceptID))
				stop(paste("Some values of 'ConceptID' are not included as",
								"taxon concept IDs in 'taxlist'."))
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
