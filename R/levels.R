# TODO:   Set hierarchy for taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# set function as generic
if(!isGeneric("levels"))
    setGeneric("levels",
            function(taxlist, levels, ...)
                standardGeneric("levels")
)

# method for taxlist objects
setMethod("levels", signature(taxlist="taxlist", levels="character"),
        function(taxlist, levels, ...) {
            if(!all(paste(taxlist@taxonRelations$Level[
                                    !is.na(taxlist@taxonRelations$Level)]) %in%
                    levels))
                stop("Some levels are not matching those indicated in slot 'taxonRelations'")
            taxlist@taxonRelations$Level <- factor(
                    paste(taxlist@taxonRelations$Level), levels=levels)
            return(taxlist)
        }
)

# Replacement for taxlist
setReplaceMethod("levels", signature(x="taxlist", value="character"),
        function(x, value) {
            x <- levels(taxlist=x, levels=value)
            return(x)
        }
)
