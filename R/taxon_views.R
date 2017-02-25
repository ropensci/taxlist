# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

setGeneric("taxon_views",
        function(taxlist, ...)
            standardGeneric("taxon_views")
)

# Set method for taxlist
setMethod("taxon_views", signature(taxlist="taxlist"),
        function(taxlist, ...) taxlist@taxonViews
)

# Replacement methods
setGeneric("taxon_views<-", function(taxlist, value)
            standardGeneric("taxon_views<-"))

# Replacement methods for the assignment to slot taxonRelations
setReplaceMethod("taxon_views", signature(taxlist="taxlist", value="numeric"),
        function(taxlist, value) {
            if(length(value) == 1)
                value <- rep_len(value, nrow(taxlist@taxonRelations))
            taxlist@taxonRelations$View <- value
            if(nrow(taxlist@taxonViews) == 0) {
                taxlist@taxonViews <- data.frame(View=unique(value),
                        row.names=paste(unique(value)))
            }
            return(taxlist)
        }
)

# Replacement methods for the assignment to slot taxonViews
setReplaceMethod("taxon_views", signature(taxlist="taxlist",
                value="data.frame"), function(taxlist, value) {
            taxlist@taxonViews <- value
            return(taxlist)
        }
)
