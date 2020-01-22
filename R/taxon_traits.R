#' @name taxon_traits
#' @aliases taxon_traits,taxlist-method taxon_traits<-
#'     taxon_traits<-,taxlist,data.frame-method update_trait
#'     update_trait,taxlist,numeric-method
#' 
#' @title Manipulation of taxon traits in taxlist objects.
#' 
#' @description 
#' The slot `taxonTraits` in \code{\linkS4class{taxlist}} objects contains
#' attributes of taxon concepts (e.g. functional traits).
#' These functions are suitable for replacing, retrieving and appending trait
#' information in taxonomic lists.
#' 
#' @param taxlist A \code{\linkS4class{taxlist}} object.
#' @param ConceptID A numeric vector with the respective taxon concept IDs.
#' @param value Data frame to be set as slot \code{'taxonTraits'}.
#' @param ... Further arguments to be passed among methods.
#' 
#' @details 
#' Taxon traits are contained in a data frame at the slot `taxonTraits` in
#' \code{\linkS4class{taxlist}} objects.
#' To optimise space, this data frame contain only entries for those concepts
#' with information, while taxa with no information are skipped from this table.
#' Thus appending new variables may also have to include new rows in this slot,
#' which is automatically carried out by this function.
#' 
#' The replacement method `taxon_traits<-` should be only used when
#' constructing \code{\linkS4class{taxlist}} objects from an empty one.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso \code{\linkS4class{taxlist}}.
#' 
#' @examples 
#' head(taxon_traits(Easplist))
#' 
#' @rdname taxon_traits
#' @export 
setGeneric("taxon_traits",
        function(taxlist, ...)
            standardGeneric("taxon_traits")
)

#' @rdname taxon_traits
#' 
#' @export 
setMethod("taxon_traits", signature(taxlist="taxlist"),
        function(taxlist, ...) taxlist@taxonTraits
)

#' @rdname taxon_traits
#' 
#' @export 
setGeneric("taxon_traits<-", function(taxlist, value)
            standardGeneric("taxon_traits<-"))

#' @rdname taxon_traits
#' 
#' @export 
setReplaceMethod("taxon_traits", signature(taxlist="taxlist",
                value="data.frame"),
        function(taxlist, value) {
            if(!"TaxonConceptID" %in% colnames(value))
                stop("'TaxonConceptID' is a mandatory field in 'value'")
            if(class(value$TaxonConceptID) != "integer")
                value$TaxonConceptID <- as.integer(value$TaxonConceptID)
            if(any(duplicated(value$TaxonConceptID))) {
                warning("duplicated concepts will be deleted from 'value'")
                value <- value[unique(value$TaxonConceptID),]
            }
            taxlist@taxonTraits <- value[value$TaxonConceptID %in%
                            taxlist@taxonRelations$TaxonConceptID,]
            return(taxlist)
        }
)

#' @rdname taxon_traits
#' 
#' @export 
setGeneric("update_trait",
		function(taxlist, ConceptID, ...)
			standardGeneric("update_trait")
)

#' @rdname taxon_traits
#' 
#' @export 
setMethod("update_trait", signature(taxlist="taxlist", ConceptID="numeric"),
		function(taxlist, ConceptID, ...) {
			if(any(!ConceptID %in% taxlist@taxonRelations$TaxonConceptID))
				stop(paste("Some values of 'ConceptID' are not included as",
								"taxon concept IDs in 'taxlist'."))
			new_entries <- list(...)
			for(i in names(new_entries)[!names(new_entries) %in%
							colnames(taxlist@taxonTraits)])
				taxlist@taxonTraits[,i] <- rep(NA, nrow(taxlist@taxonTraits))
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
