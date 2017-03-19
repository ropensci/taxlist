# TODO:   Coersion to list
# 
# Author: Miguel Alvarez
################################################################################

S4_to_list <- function(x) {
    out <- list()
    for(i in slotNames(x)) out[[i]] <- slot(x, i)
    return(out)
}

# Method for taxlist
setMethod("as.list", signature(x="taxlist"),
        function(x, ...) {
            S4_to_list(x)
        }
)
