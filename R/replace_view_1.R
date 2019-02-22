# TODO:   Change information related to a taxon usage name
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("update_name",
		function(taxlist, UsageID, ...)
			standardGeneric("update_name")
)

# Method for taxlist objects
setMethod("update_name", signature(taxlist="taxlist", UsageID="numeric"),
		function(taxlist, UsageID, ...) {
			new_entries <- as.data.frame(list(...), stringsAsFactors=FALSE)
			if(length(UsageID) != nrow(new_entries))
				stop("Length of 'UsageID' is not matching the length of corrected entries.")
			if(any(!UsageID %in% taxlist@taxonNames$TaxonUsageID))
				stop("Some values of 'UsageID' are not included as taxon usage names in 'taxlist'.")
			if(any(!colnames(new_entries) %in% colnames(taxlist@taxonNames)))
				stop("Some of the indicated variables are not included in 'taxlist' (slot TaxonNames).")
			for(i in colnames(new_entries))
				taxlist@taxonNames[match(UsageID,
								taxlist@taxonNames$TaxonUsageID),
						i] <- new_