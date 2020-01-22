#' @name taxon_names
#' @aliases taxon_names,taxlist-method
#' @aliases taxon_names<-
#' @aliases taxon_names<-,taxlist,data.frame-method
#' @aliases add_synonym
#' @aliases add_synonym,taxlist-method
#' @aliases update_name
#' @aliases update_name,taxlist,numeric-method
#' @aliases delete_name
#' @aliases delete_name,taxlist,numeric-method
#' 
#' @title Handle information on taxon usage names.
#' 
#' @description 
#' The slot `taxonNames` in \code{\linkS4class{taxlist}} objects contains
#' taxon usage names for the respective taxon.
#' These functions assist on the access and modification of entries for names.
#' 
#' @param taxlist A \code{\linkS4class{taxlist}} object to be modified.
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
#' names in empty \code{\linkS4class{taxlist}} objects.
#' 
#' The function \code{'add_synonym'} works only for adding names to existing
#' taxon concepts.
#' For adding new taxon concepts as well you should use
#' \code{\link{add_concept}}.
#' 
#' @return A data frame or, in the case of the replacement method, a
#' \code{\linkS4class{taxlist}} object with modified slot \code{'taxonNames'}.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso \code{\linkS4class{taxlist}}.
#' 
#' @examples 
#' ## Display of slot 'taxonNames'
#' Euclea <- subset(Easplist, charmatch("Euclea", TaxonName), slot="names",
#'     keep_children=TRUE)
#' summary(Euclea)
#' taxon_names(Euclea)
#' 
#' ## Insert a synonym to Diospyros scabra
#' summary(Easplist, "Diospyros scabra")
#' Easplist <- add_synonym(Easplist, 51793, TaxonName="Maba scabra",
#'     AuthorName="Chiov.")
#' summary(Easplist, "Diospyros scabra")
#' 
#' ## Delete a synonym of Launaea cornuta
#' summary(Easplist, "Launaea cornuta")
#' Easplist <- delete_name(Easplist, 53821)
#' summary(Easplist, "Launaea cornuta")
#' 
#' @rdname taxon_names
#' @export 
setGeneric("taxon_names",
        function(taxlist, ...)
            standardGeneric("taxon_names")
)

#' @rdname taxon_names
#' 
#' @export 
setMethod("taxon_names", signature(taxlist="taxlist"),
        function(taxlist, ...) taxlist@taxonNames
)

#' @rdname taxon_names
#' 
#' @export 
setGeneric("taxon_names<-", function(taxlist, value)
            standardGeneric("taxon_names<-"))

#' @rdname taxon_names
#' 
#' @export 
setReplaceMethod("taxon_names", signature(taxlist="taxlist",
                value="data.frame"), function(taxlist, value) {
            taxlist@taxonNames <- value
            return(taxlist)
        }
)

#' @rdname taxon_names
#' 
#' @export 
setGeneric("add_synonym",
		function(taxlist, ConceptID, ...)
			standardGeneric("add_synonym")
)

#' @rdname taxon_names
#' 
#' @export 
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
				taxlist@taxonNames[,i] <- NA
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
#' @export 
setGeneric("update_name",
		function(taxlist, UsageID, ...)
			standardGeneric("update_name")
)

#' @rdname taxon_names
#' 
#' @export 
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
						i] <- new_entries[,i]
			return(taxlist)
		}
)

#' @rdname taxon_names
#' 
#' @export 
setGeneric("delete_name",
		function(taxlist, UsageID, ...)
			standardGeneric("delete_name")
)

#' @rdname taxon_names
#' 
#' @export 
setMethod("delete_name", signature(taxlist="taxlist", UsageID="numeric"),
		function(taxlist, UsageID, ...) {
			if(any(UsageID %in% taxlist@taxonRelations$AcceptedName))
				stop(paste("Values in 'UsageID' are not allowed to be",
								"accepted names in 'taxlist'."))
			if(any(UsageID %in% taxlist@taxonRelations$Basionym))
				stop(paste("Values in 'UsageID' are not allowed to be",
								"basionyms in 'taxlist'."))
			taxlist@taxonNames <- taxlist@taxonNames[
					!taxlist@taxonNames$TaxonUsageID %in% UsageID,]
			return(taxlist)
		}
)
