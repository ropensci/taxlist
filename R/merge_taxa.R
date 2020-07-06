#' @name merge_taxa
#' 
#' @title Merge concepts or move names
#' 
#' @description 
#' Merge taxon concepts form a [taxlist-class] object into single ones.
#' 
#' @param object,taxlist Object of class [taxlist-class].
#' @param concepts Numeric (integer) vector including taxon concepts to be
#'     merged.
#' @param level Character vector indicating the lowest level for merging.
#' @param print_output Logical value indicating whether the merged concept
#'     should be displayed in the console.
#' @param UsageID Numeric vector with taxon usage IDs to be changed from
#'     concept.
#' @param value Numeric vector with taxon concept IDs to be assigned to the
#'     names.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @details 
#' Taxon concepts indicated in argument `concepts` will be merged into a
#' single concept.
#' The new concept inherits the ID and respective attributes from slots
#' `taxonRelations` and `taxonTraits` from the first taxon concept
#' indicated in argument `concepts`.
#' 
#' For convenience the resulting concept can be displayed by setting
#' `print_output=TRUE` but only when using argument `concepts`.
#' 
#' An alternative application of this function is implemented through the
#' argument `level`, where all lower rank taxa will be merged to the indicated
#' level or higher (if parent of merged taxa are at a higher rank).
#' 
#' @return An object of class [taxlist-class].
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples 
#' ## Merge Cyperus papyrus and Cyperus dives
#' summary(Easplist, c(206, 197))
#'
#' Easplist <- merge_taxa(object=Easplist, concepts=c(206, 197),
#'     print_output=TRUE)
#'
#' ## Move the name Typha aethiopica to concept 573 (T. latifolia)
#' change_concept(Easplist, 53130) <- 573
#' summary(Easplist, c(50105,573))
#' 
#' @rdname merge_taxa
#' 
#' @exportMethod merge_taxa
#' 
setGeneric("merge_taxa",
		function(object, concepts, level, ...)
			standardGeneric("merge_taxa")
)

#' @rdname merge_taxa
#' 
#' @aliases merge_taxa,taxlist,numeric,missing-method
setMethod("merge_taxa", signature(object="taxlist", concepts="numeric",
				level="missing"),
		function(object, concepts, print_output=FALSE, ...) {
			# Tests previous running function
			if(!length(concepts) > 1)
				stop("Argument 'concepts' must have a length > 1")
			if(any(!concepts %in% object@taxonRelations$TaxonConceptID))
				stop(paste("All values in 'concepts' should be included as",
								"concepts in 'object'"))
			# Merging concepts
			object@taxonNames[object@taxonNames$TaxonConceptID %in% concepts,
					"TaxonConceptID"] <- concepts[1]
			object@taxonRelations[object@taxonRelations$Parent %in% concepts,
					"Parent"] <- concepts[1]
			object@taxonRelations <- object@taxonRelations[
					!object@taxonRelations$TaxonConceptID %in% concepts[-1], ]
			object <- clean(object)
			# Print result#
			if(print_output) {
				summary(object, concepts[1])
			}
			# Return modified object
			return(object)
		}
)

#' @rdname merge_taxa
#' 
#' @aliases merge_taxa,taxlist,missing,character-method
#' 
setMethod("merge_taxa", signature(object="taxlist", concepts="missing",
				level="character"),
		function(object, level, ...) {
			if(!level %in% paste(levels(object)))
				stop("The requested 'level' is not included in 'object'")
			for(i in paste(levels(object))[1:(which(paste(levels(object)) ==
									level) - 1)]) {
				DEL <- object@taxonRelations[
						paste(object@taxonRelations$Level) == i,
						"TaxonConceptID"]
				REPL <- object@taxonRelations[
						paste(object@taxonRelations$Level) == i, "Parent"]
				object@taxonNames[object@taxonNames$TaxonConceptID %in% DEL,
						"TaxonConceptID"] <- REPL[
								match(object@taxonNames[
												object@taxonNames$TaxonConceptID %in%
														DEL, "TaxonConceptID"],
										DEL)]
				object@taxonRelations <- object@taxonRelations[
						!object@taxonRelations$TaxonConceptID %in% DEL, ]
			}
		return(clean(object))
		}
)

#' @rdname merge_taxa
#' 
#' @aliases change_concept<-
#' 
#' @exportMethod change_concept<-
#' 
setGeneric("change_concept<-", function(taxlist, UsageID, value)
			standardGeneric("change_concept<-"))

#' @rdname merge_taxa
#' 
#' @aliases change_concept<-,taxlist-method
#' 
setReplaceMethod("change_concept", signature(taxlist="taxlist"),
		function(taxlist, UsageID, value) {
			# Test
			if(length(UsageID) != length(value))
				stop("'UsageID' and 'value' should be of the same length")
			if(any(UsageID %in% taxlist@taxonRelations$AcceptedName))
				stop("Changes on concept are not allowed for accepted names")
			# now replace
			taxlist@taxonNames[match(UsageID,taxlist@taxonNames$TaxonUsageID),
					"TaxonConceptID"] <- value
			return(taxlist)
		}
)
