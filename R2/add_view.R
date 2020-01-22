# TODO:   Add a new view to a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

# Set generic method
setGeneric("add_view",
        function(taxlist, ...)
            standardGeneric("add_view")
)

# Method for taxlist
setMethod("add_view", signature(taxlist="taxlist"),
        function(taxlist, ...) {
            if(nrow(taxlist@taxonViews) == 0) ViewID <- 1 else
                ViewID <- max(taxlist@taxonViews$ViewID) + 1
            new_view <- list(...)
            ViewID <- ViewID:(ViewID + length(new_view[[1]]) - 1)
            new_view <- list(ViewID=ViewID, ...)
            for(i in colnames(taxlist@taxonViews)[
					!colnames(taxlist@taxonViews) %in% names(new_view)]) {
                new_view[[i]] <- rep(NA, length(ViewID))
            }
			new_view <- as.data.frame(new_view, stringsAsFactors=FALSE)
			if(nrow(taxlist@taxonViews) > 0) {
				old_view <- taxlist@taxonViews
				for(i in colnames(new_view)[!colnames(new_view) %in%
								colnames(old_view)]) {
					old_view[,i] <- rep(NA, nrow(old_view))
				}
				new_view <- do.call(rbind, list(old_view, new_view))
			}
			taxlist@taxonViews <- new_view
            return(taxlist)
        }
)
