# TODO:   A method for replacing view IDs in taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# Set generic method
setGeneric("replace_view",
		function(taxlist, old_view, new_view, ...)
			standardGeneric("replace_view")
)

# Set taxlist method
setMethod("replace_view", signature(taxlist="taxlist", old_view="numeric",
				new_view="numeric"),
		function(taxlist, old_view, new_view, ...) {
			if(length(old_view) != length(new_view))
				stop("Lenght of 'old_view' must be equal to 'new_view'.")
			if(!old_view %in% taxlist@taxonViews$ViewID)
				stop("Value of 'old_view' is not a view ID in 'taxlist' object.")
			if(!new_view %in% taxlist@taxonViews$ViewID)
				stop("Value of 'new_view' is not a view ID in 'taxlist' object.")
			taxlist@taxonRelations$ViewID[taxlist@taxonRelations$ViewID %in%
							old_view] <- new_view[match(taxlist@taxonRelations$ViewID[
											taxlist@taxonRelations$ViewID %in%
													old_view], old_view)]
			return(taxlist)
		}
)
