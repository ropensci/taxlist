# TODO:   Set hierarchy for taxlist objects
# 
# Author: Miguel Alvarez
################################################################################

# set function as generic
if(!isGeneric("levels"))
    setGeneric("levels",
            function(x, ...)
                standardGeneric("levels")
)

# method for factor objects (to left levels accessible for factors)
setMethod("levels", signature(x="factor"),
        function(x, ...) {
            base::levels(x)
        }
)

# method for taxlist objects
setMethod("levels", signature(x="taxlist"),
        function(x, ...) {
            if(class(x@taxonRelations$Level) != "factor")
                base::levels(as.factor(x@taxonRelations$Level)) else
                base::levels(x@taxonRelations$Level)
        }
)

# Replacement for taxlist
setReplaceMethod("levels", signature(x="taxlist", value="character"),
        function(x, value) {
            if(!all(paste(x@taxonRelations$Level[
                                            !is.na(x@taxonRelations$Level)
                    ]) %in% value))
                stop("Some levels are not matching those indicated in slot 'taxonRelations'")
            x@taxonRelations$Level <- factor(
                    paste(x@taxonRelations$Level), levels=value)
            return(x)
        }
)
