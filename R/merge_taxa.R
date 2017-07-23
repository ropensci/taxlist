# TODO:   Functions for merging and splitting taxa
# 
# Author: Miguel Alvarez
################################################################################

# Merging taxa -----------------------------------------------------------------

# Generic function
setGeneric("merge_taxa",
		function(object, concepts, ...)
			standardGeneric("merge_taxa")
)

# Method for 'taxlist' object
setMethod("merge_taxa", signature(object="taxlist", concepts="numeric"),
		function(object, concepts, print_output=FALSE, ...) {
			# Tests previous running function
			if(!length(concepts) > 1)
				stop("Argument 'concepts' must have a length > 1")
			if(any(!concepts %in% object@taxonRelations$TaxonConceptID))
				stop("All values in 'concepts' should be included as concepts in 'object'")
			# Merging concepts
			object@taxonNames[object@taxonNames$TaxonConceptID %in% concepts, "TaxonConceptID"] <- concepts[1]
			object@taxonRelations <- object@taxonRelations[
					!object@taxonRelations$TaxonConceptID %in% concepts[-1],]
			object <- clean(object)
			# Print result#
			if(print_output) {
				summary(object, concepts[1])
			}
			# Return modified object
			return(object)
		}
)

# Split taxa may follows
