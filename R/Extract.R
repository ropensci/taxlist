#' @name Extract
#' 
#' @title Extract or Replace Parts of taxlist Objects
#' 
#' @description 
#' Quick access to slots `taxonTraits` and `taxonRelations` within
#' [taxlist-class] objects.
#' 
#' @param x Object of class [taxlist-class].
#' @param i Integer or logical vector used as index for access to taxon
#'   concepts, referring to the rows in slot 'taxonRelations'. These indices can
#'   be used to produce a object with a subset of taxon concepts. It is not
#'   recommended to use character values for this index.
#' @param j Integer, logical or character vector used as index for access to
#'   variables in slot 'taxonTraits'. These indices can be used to reduce the
#'   number of variables in the mentioned slot.
#' @param drop A logical value passed to \code{\link[base]{Extract}}.
#' @param name A symbol or character value for the method `$`, corresponding to
#'   a variable either at slot 'taxonTraits' or slot 'taxonRelations'.
#' 
#' @return The method `$` retrieves a vector, while `[` retrieves a subset
#' of the input [taxlist-class] object.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}.
#' 
#' @seealso [taxlist-class] \code{\link[taxlist]{subset}}
#' 
#' @examples 
#' ## Statistics on life forms
#' summary(as.factor(Easplist$lf_behn_2018))
#' 
#' ## First ten concepts in this list
#' summary(Easplist[1:10, ], "all")
#' 
#' @rdname Extract
#' 
#' @aliases [ [,taxlist-method
#' 
#' @exportMethod [
#' 
setMethod("[", signature(x="taxlist"),
		function(x, i, j, drop=FALSE) {
			if(missing(i)) i <- TRUE
			if(missing(j)) j <- TRUE
			# Resolving problems with NAs
			if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
			if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
			x@taxonRelations <- x@taxonRelations[i,,drop]
			if(nrow(x@taxonTraits) > 0) {
				## k <- with(x@taxonTraits, TaxonConceptID %in%
				##                 x@taxonRelations$TaxonConceptID)
				k <- x@taxonTraits$TaxonConceptID %in%
						x@taxonRelations$TaxonConceptID
				x@taxonTraits <- x@taxonTraits[k,j,drop]
			}
			return(clean(x))
		}
)

#' @rdname Extract
#' 
#' @aliases $ $,taxlist-method
#' 
#' @exportMethod $
#' 
setMethod("$", signature(x="taxlist"), function(x, name) {
			pos_nms <- c(colnames(x@taxonTraits), colnames(x@taxonRelations))
			if(!name %in% pos_nms)
				stop(paste("Only variables in slots 'taxonTraits' or",
								"'taxonRelations' can be accessed by '$'"))
			if(!paste(substitute(name)) %in% colnames(x@taxonRelations))
				## x@taxonRelations[[name]] <- with(x@taxonTraits,
				##         get(name)[match(x@taxonRelations$TaxonConceptID,
				##                         TaxonConceptID)])
				x@taxonRelations[[name]] <- x@taxonTraits[
						match(x@taxonRelations$TaxonConceptID,
								x@taxonTraits$TaxonConceptID), name]
			return(x@taxonRelations[[name]])
        }
)
