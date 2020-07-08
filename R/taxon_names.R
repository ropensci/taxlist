#' @name taxon_names
#' 
#' @title Handle information on taxon usage names.
#' 
#' @description 
#' The slot `taxonNames` in [taxlist-class] objects contains
#' taxon usage names for the respective taxon.
#' These functions assist on the access and modification of entries for names.
#' 
#' @param taxlist A [taxlist-class] object to be modified.
#' @param value A data frame used as new slot `taxonNames` in `taxlist`.
#' @param ConceptID Numeric vector indicating the concept ID to which the
#'     synonyms will be added.
#' @param TaxonName,AuthorName Character values used for the new names
#'     (synonyms).
#' @param UsageID Numeric vector indicating the taxon usage IDs to be updated.
#' @param ... Further arguments passed among methods. In `update_name` are
#'     vectors including the variables to be updated for the respective taxon
#'     usage ID.
#' 
#' @details 
#' The replacement method `taxon_names<-` is a quick alternative to include
#' names in empty [taxlist-class] objects.
#' 
#' The function `add_synonym()` works only for adding names to existing
#' taxon concepts.
#' For adding new taxon concepts as well you should use [add_concept()].
#' 
#' @return A data frame or, in the case of the replacement method, a
#' [taxlist-class] object with modified slot `taxonNames`.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [taxlist-class]
#' 
#' @examples 
#' ## Display of slot 'taxonNames'
#' Euclea <- subset(x=Easplist, subset=charmatch("Euclea", TaxonName),
#'     slot="names", keep_children=TRUE)
#' Euclea
#' taxon_names(Euclea)
#' 
#' ## Insert a synonym to Diospyros scabra
#' summary(Easplist, "Diospyros scabra")
#' Easplist <- add_synonym(taxlist=Easplist, ConceptID=51793,
#'     TaxonName="Maba scabra", AuthorName="Chiov.")
#' summary(Easplist, "Diospyros scabra")
#' 
#' ## Delete a synonym of Launaea cornuta
#' summary(Easplist, "Launaea cornuta")
#' Easplist <- delete_name(Easplist, 53821)
#' summary(Easplist, "Launaea cornuta")
#' 
#' ## Hypothetical correction in author name in Launaea cornuta
#' Easplist <- update_name(taxlist=Easplist, UsageID=355, AuthorName="L.")
#' summary(Easplist, "Launaea cornuta")
#' 
#' @rdname taxon_names
#' 
#' @exportMethod taxon_names
#' 
setGeneric("taxon_names",
        function(taxlist, ...)
            standardGeneric("taxon_names")
)

#' @rdname taxon_names
#' 
#' @aliases taxon_names,taxlist-method
#' 
setMethod("taxon_names", signature(taxlist="taxlist"),
        function(taxlist, ...) taxlist@taxonNames
)

#' @rdname taxon_names
#' 
#' @aliases taxon_names<-
#' 
#' @exportMethod taxon_names<-
#' 
setGeneric("taxon_names<-", function(taxlist, value)
            standardGeneric("taxon_names<-"))

#' @rdname taxon_names
#' 
#' @aliases taxon_names<-,taxlist,data.frame-method
#' 
setReplaceMethod("taxon_names", signature(taxlist="taxlist",
                value="data.frame"), function(taxlist, value) {
            taxlist@taxonNames <- value
            return(taxlist)
        }
)

#' @rdname taxon_names
#' 
#' @aliases add_synonym
#' 
#' @exportMethod add_synonym
#' 
setGeneric("add_synonym",
		function(taxlist, ConceptID, ...)
			standardGeneric("add_synonym")
)

#' @rdname taxon_names
#' 
#' @aliases add_synonym,taxlist-method
#' 
setMethod("add_synonym", signature(taxlist="taxlist"),
		function(taxlist, ConceptID, TaxonName, AuthorName, ...) {
			if(!all(ConceptID %in% taxlist@taxonRelations$TaxonConceptID))
				stop(paste("Some values in 'ConceptID' are not included as",
								"concepts in 'taxlist'"))
			# For addition of multiple synonyms to multiple concepts
			if(length(ConceptID) == 1) rep(ConceptID, length(TaxonName))
			TaxonConceptID <- ConceptID
			TaxonUsageID <- max(taxlist@taxonNames$TaxonUsageID) + 1
			TaxonUsageID <- TaxonUsageID:(TaxonUsageID + length(TaxonName) - 1)
			new_name <- list(TaxonConceptID=TaxonConceptID,
					TaxonUsageID=TaxonUsageID, TaxonName=TaxonName,
					AuthorName=AuthorName, ...)
			for(i in colnames(taxlist@taxonNames)[
					!colnames(taxlist@taxonNames) %in% names(new_name)])
				new_name[[i]] <- rep(NA, length(new_name$TaxonConceptID))
			for(i in names(new_name)[!names(new_name) %in%
							colnames(taxlist@taxonNames)])
				taxlist@taxonNames[ ,i] <- NA
			taxlist@taxonNames <- do.call(rbind,
					list(taxlist@taxonNames,
							new_name[match(colnames(taxlist@taxonNames),
											names(new_name))],
							stringsAsFactors=FALSE))
			return(taxlist)
		}
)

#' @rdname taxon_names
#' 
#' @aliases update_name
#' 
#' @exportMethod update_name
#' 
setGeneric("update_name",
		function(taxlist, UsageID, ...)
			standardGeneric("update_name")
)

#' @rdname taxon_names
#' 
#' @aliases update_name,taxlist,numeric-method
#' 
setMethod("update_name", signature(taxlist="taxlist", UsageID="numeric"),
		function(taxlist, UsageID, ...) {
			new_entries <- as.data.frame(list(...), stringsAsFactors=FALSE)
			if(length(UsageID) != nrow(new_entries))
				stop(paste("Length of 'UsageID' is not matching the length",
								"of corrected entries."))
			if(any(!UsageID %in% taxlist@taxonNames$TaxonUsageID))
				stop(paste("Some values of 'UsageID' are not included as",
								"taxon usage names in 'taxlist'."))
			if(any(!colnames(new_entries) %in% colnames(taxlist@taxonNames)))
				stop(paste("Some of the indicated variables are not included",
								"in 'taxlist' (slot TaxonNames)."))
			for(i in colnames(new_entries))
				taxlist@taxonNames[match(UsageID,
								taxlist@taxonNames$TaxonUsageID),
						i] <- new_entries[ ,i]
			return(taxlist)
		}
)

#' @rdname taxon_names
#' 
#' @aliases delete_name
#' 
#' @exportMethod delete_name
#' 
setGeneric("delete_name",
		function(taxlist, UsageID, ...)
			standardGeneric("delete_name")
)

#' @rdname taxon_names
#' 
#' @aliases delete_name,taxlist,numeric-method
#' 
setMethod("delete_name", signature(taxlist="taxlist", UsageID="numeric"),
		function(taxlist, UsageID, ...) {
			if(any(UsageID %in% taxlist@taxonRelations$AcceptedName))
				stop(paste("Values in 'UsageID' are not allowed to be",
								"accepted names in 'taxlist'."))
			if(any(UsageID %in% taxlist@taxonRelations$Basionym))
				stop(paste("Values in 'UsageID' are not allowed to be",
								"basionyms in 'taxlist'."))
			taxlist@taxonNames <- taxlist@taxonNames[
					!taxlist@taxonNames$TaxonUsageID %in% UsageID, ]
			return(taxlist)
		}
)
