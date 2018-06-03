# TODO:   Change information related to a taxon usage name
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("update_name",
		function(object, UsageID, ...)
			standardGeneric("update_name")
)

# Method for taxlist objects
setMethod("update_name", signature(object="taxlist", UsageID="numeric"),
		function(object, UsageID, ...) {
			new_entries <- as.data.frame(list(...), stringsAsFactors=FALSE)
			if(length(UsageID) != nrow(new_entries))
				stop("Length of 'UsageID' is not matching the length of corrected entries.")
			if(any(!UsageID %in% object@taxonNames$TaxonUsageID))
				stop("Some values of 'UsageID' are not included as taxon usage names in 'object'.")
			if(any(!colnames(new_entries) %in% colnames(object@taxonNames)))
				stop("Some of the indicated variables are not included in 'object' (slot TaxonNames).")
			for(i in colnames(new_entries))
				object@taxonNames[match(UsageID,
								object@taxonNames$TaxonUsageID),
						i] <- new_entries[,i]
			return(object)
		}
)
