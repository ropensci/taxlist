# TODO:   Specific methods for slot taxonRelations
# 
# Author: Miguel Alvarez
################################################################################

setGeneric("taxon_relations",
        function(taxlist, ...)
            standardGeneric("taxon_relations")
)

# Set method for taxlist
setMethod("taxon_relations", signature(taxlist="taxlist"),
        function(taxlist, ...) taxlist@taxonRelations
)

# Replacement methods
setGeneric("taxon_relations<-", function(taxlist, value)
            standardGeneric("taxon_relations<-"))

# Replacement for taxlist
setReplaceMethod("taxon_relations", signature(taxlist="taxlist",
                value="data.frame"), function(taxlist, value) {
            taxlist@taxonRelations <- value
            return(taxlist)
        }
)
