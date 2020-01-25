#' @name Extract
#' @aliases [ [,taxlist-method $,taxlist-method
#' 
#' @title Extract or Replace Parts of taxlist Objects
#' 
#' @description 
#' Quick access to slots `taxonTraits` and `taxonRelations` within
#' \code{\linkS4class{taxlist}} objects.
#' 
#' @param x Object of class \code{\linkS4class{taxlist}}.
#' @param name A name to access.
#' @param i,j Indices for access.
#' @param drop A logical value passed to \code{\link[base]{Extract}}.
#' 
#' @details 
#' While the method `$` automatically recognizes the slot queried, provided
#' that there is no shared column names.
#' 
#' In the method `[`, the first index is referred to the rows in slot
#' `taxonRelations`, while the second index indicate the columns in slot
#' `taxonTraits`.
#' 
#' A replacement method `$<-` is also implemented.
#' 
#' @return The method `$` retrieves a vector, while `[` retrieves a subset
#' of the input \code{\linkS4class{taxlist}} object.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}.
#' 
#' @seealso \code{\linkS4class{taxlist}} \code{\link[taxlist]{subset}}
#' 
#' @examples 
#' ## Statistics on life forms
#' summary(as.factor(Easplist$lf_behn_2018))
#' 
#' ## First ten concepts in this list
#' summary(Easplist[1:10,], "all")
#' 
#' @rdname Extract
#' @export 
setMethod("[", signature(x="taxlist"),
		function(x, i, j, drop=FALSE) {
			if(missing(i)) i <- TRUE
			if(missing(j)) j <- TRUE
			# Resolving problems with NAs
			if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
			if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
			x@taxonRelations <- x@taxonRelations[i,,drop]
			if(nrow(x@taxonTraits) > 0) {
				k <- with(x@taxonTraits, TaxonConceptID %in%
								x@taxonRelations$TaxonConceptID)
				x@taxonTraits <- x@taxonTraits[k,j,drop]
			}
			return(clean(x))
		}
)

#' @rdname Extract
#' 
#' @export 
setMethod("$", signature(x="taxlist"), function(x, name) {
			if(!paste(substitute(name)) %in% colnames(x@taxonRelations))
				x@taxonRelations[[name]] <- with(x@taxonTraits,
						get(name)[match(x@taxonRelations$TaxonConceptID,
										TaxonConceptID)])
			return(x@taxonRelations[[name]])
        }
)
