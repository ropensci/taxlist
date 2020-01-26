#' @name levels
#' @aliases levels,taxlist-method levels<- levels<-,taxlist-method
#' 
#' @title Set and retrieves hierarchical levels
#' 
#' @description 
#' Taxonomic hierarchies can be set as levels in \code{\linkS4class{taxlist}}
#' objects, ordered from lower to higher levels.
#' 
#' Add taxonomic levels for specific taxon concepts in a
#' \code{\linkS4class{taxlist}} object.
#' Also changes in concept circumscription may implicate changes in its
#' taxonomic hierarchy.
#' 
#' @param x A \code{\linkS4class{taxlist}} object.
#' @param value A character vector with replacement values for levels o `x`.
#' @param ... Additional arguments passed among methods.
#' 
#' @details 
#' Taxonomic levels will be handled as factors in the
#' \code{\linkS4class{taxlist}} objects.
#' Those levels are useful for creating subsets of related groups (e.g. by
#' functions \code{\link{get_children}} or \code{\link{get_parents}}).
#' 
#' Levels in combination to parent-child relationships will be further used for
#' checking consistency of taxonomic lists.
#' 
#' A replacement method of the form `levels(x) <- value` it is also implemented.
#' 
#' @return A `character` vector or a \code{\linkS4class{taxlist}} object with
#' added or modified taxonomic levels.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples 
#' ## Get levels of species list
#' taxlist::levels(Easplist)
#' 
#' ## Add aggregate as new taxonomic level
#' levels(Easplist) <- c("form","variety","subspecies","species",
#'     "complex", "aggregate","genus","family")
#' summary(Easplist)
#' 
#' @rdname levels
#' @export 
if(!isGeneric("levels"))
    setGeneric("levels",
            function(x, ...)
                standardGeneric("levels")
)

#' @rdname levels
#' 
#' @export 
setMethod("levels", signature(x="taxlist"),
        function(x, ...) {
            if(class(x@taxonRelations$Level) != "factor")
				stop("Variable 'Level' in slot taxonRelations is not a factor")
			base::levels(x@taxonRelations$Level)
        }
)

#' @rdname levels
#' 
#' @export 
setReplaceMethod("levels", signature(x="taxlist"),
		function(x, value) {
			if(!all(paste(x@taxonRelations$Level[
									!is.na(x@taxonRelations$Level)
							]) %in% value))
				stop(paste("Some levels are not matching those indicated in",
								"slot 'taxonRelations'"))
			x@taxonRelations$Level <- factor(
					paste(x@taxonRelations$Level), levels=value)
			return(x)
		}
)
