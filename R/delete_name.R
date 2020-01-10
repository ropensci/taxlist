# TODO:   A method for deleting names in taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# Set generic method
setGeneric("delete_name",
		function(taxlist, UsageID, ...)
			standardGeneric("delete_name")
)

# Method for taxlist
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
