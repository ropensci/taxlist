# TODO:   Counting taxa by their names
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("count_taxa",
		function(object, ...)
			standardGeneric("count_taxa")
)

# Method for character vectors
setMethod("count_taxa", signature(object="character"),
		function(object, rm.na=TRUE, ...) {
			if(rm.na) object <- object[!is.na(object)]
			return(length(unique(object)))
		}
)

# Method for factors
setMethod("count_taxa", signature(object="factor"),
		function(object, rm.na=TRUE, ...) {
			if(rm.na) object <- object[!is.na(object)]
			return(count_taxa(paste(object)))
		}
)

# Method for taxlist objects
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
