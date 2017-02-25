# TODO:   Specific methods for slot taxonNames
# 
# Author: Miguel Alvarez
################################################################################

setGeneric("taxon_names",
        function(taxlist, ...)
            standardGeneric("taxon_names")
)

# Set method for taxlist
setMethod("taxon_names", signature(taxlist="taxlist"),
        function(taxlist, ...) taxlist@taxonNames
)

# Replacement methods
setGeneric("taxon_names<-", function(taxlist, value)
            standardGeneric("taxon_names<-"))

# Replacement for taxlist
setReplaceMethod("taxon_names", signature(taxlist="taxlist",
                value="data.frame"), function(taxlist, value) {
            taxlist@taxonNames <- value
            return(taxlist)
        }
)
