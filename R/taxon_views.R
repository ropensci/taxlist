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

# Replacement methods for the assignment to slot taxonViews
setReplaceMethod("taxon_views", signature(taxlist="taxlist",
                value="data.frame"), function(taxlist, value) {
            taxlist@taxonViews <- value
            return(taxlist)
        }
)
