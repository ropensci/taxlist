#' @name count_taxa
#' @aliases count_taxa,character-method count_taxa,factor-method
#'     count_taxa,taxlist-method
#' 
#' @title Count taxa within a taxlist object.
#' 
#' @description 
#' Counting number of taxa within \code{\linkS4class{taxlist}} objects or
#' character vectors containing taxon names.
#' 
#' @param object An object of class \code{\linkS4class{taxlist}}.
#' @param rm.na Logical value, whether NAs have to be removed from the input
#'     vector or not.
#' @param level Character value indicating the taxonomic rank of counted taxa.
#' @param ... further arguments passed among methods.
#' 
#' @details 
#' This function is written by convenience in order to reduce code for counting
#' taxa within \code{\linkS4class{taxlist}} objects and it is just a wrapper of
#' \code{\link{length}}.
#' 
#' @return An integer with the number of taxa.
#' 
#' @author Miguel Alvarez.
#' 
#' @examples 
#' library(taxlist)
#' 
#' ## factor method
#' count_taxa(iris$Species)
#' 
#' ## taxlist method
#' count_taxa(Easplist)
#' count_taxa(Easplist, level="species")
#' 
#' @rdname count_taxa
#' @export 
setGeneric("count_taxa",
		function(object, ...)
			standardGeneric("count_taxa")
)

#' @rdname count_taxa
#' 
#' @export 
setMethod("count_taxa", signature(object="character"),
		function(object, rm.na=TRUE, ...) {
			if(rm.na) object <- object[!is.na(object)]
			return(length(unique(object)))
		}
)

#' @rdname count_taxa
#' 
#' @export 
setMethod("count_taxa", signature(object="factor"),
		function(object, rm.na=TRUE, ...) {
			if(rm.na) object <- object[!is.na(object)]
			return(count_taxa(paste(object)))
		}
)

#' @rdname count_taxa
#' 
#' @export 
setMethod("count_taxa", signature(object="taxlist"),
		function(object, level, ...) {
			if(missing(level))
				n_taxa <- nrow(object@taxonRelations) else {
				if(!level %in% levels(object))
					stop(paste("Value of argument 'level' is not a level in",
									"'object'."))
				n_taxa <- nrow(object@taxonRelations[
								paste(object@taxonRelations$Level) == level,])
			}
			return(n_taxa)
		}
)
